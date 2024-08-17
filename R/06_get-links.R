library(dplyr)

# Criteria:
# * registry ctgov or drks
# * full results journal article
# * publication has pmid (may or may not have doi)
# * publication full-text available
# * earliest publication (if multiple full results with pmid associated with trial)
# * best linked trn (if multiple trns associate with trial)

# Prepare direcct registrations and results -------------------------------

results <- readr::read_csv(here::here("data", "processed", "results.csv"))
registrations <- readr::read_csv(here::here("data", "raw", "registrations.csv"))

results_registrations <-
  results |> 
  
  filter(pub_type == "full_results_journal_article") |> 
  
  # Limit to results with pmid, since needed for some links analyses
  filter(!is.na(pmid)) |> 
  
  # Add in all trn for each trial so multiple rows per trial (i.e., multiple results connected to multiple trials, and multiple trn connected to single trial)
  left_join(registrations, by = "id", relationship = "many-to-many") |> 
  
  select(id, doi, pmid, date_publication, trn, registry)

# Get links: publication in registration ----------------------------------

# Extract publication links from registrations
ctgov_ref <- 
  readr::read_rds(here::here("data", "processed", "registries", "ctgov", "ctgov-references.rds")) |> 
  mutate(doi = tolower(doi)) |> 
  select(trn = nct_id, doi, pmid, reference_derived) |>
  filter(!is.na(doi) | !is.na(pmid)) |> 
  mutate(
    doi_link = if_else(!is.na(doi), TRUE, FALSE, missing = FALSE), 
    pmid_link = if_else(!is.na(pmid), TRUE, FALSE, missing = FALSE)
  )

ref_links <-
  results_registrations |>
  
  # DOI linked
  left_join(select(ctgov_ref, trn, doi, doi_link, reference_derived), by = c("trn", "doi")) |> 
  
  # PMID linked
  left_join(select(ctgov_ref, trn, pmid, pmid_link, reference_derived), by = c("trn", "pmid")) |> 
  
  mutate(reference_derived = coalesce(reference_derived.x, reference_derived.y), .keep = "unused") |> 
  
  mutate(has_reg_pub_link = if_else(doi_link|pmid_link, TRUE, FALSE), .before = "doi_link") |>
  
  mutate(across(ends_with("link"), ~tidyr::replace_na(., FALSE)))

# Note: FALSE logically applies to ClinicalTrials.gov only

# Get links: trn in secondary id, abstract, full-text ---------------------

# renv::install("maia-sh/ctregistries")
library(ctregistries)
# renv::install("maia-sh/tidypubmed")
library(tidypubmed)

source("https://github.com/maia-sh/intovalue-data/raw/main/scripts/functions/extract_pubmed.R")
source("https://github.com/maia-sh/intovalue-data/raw/main/scripts/functions/get_grobid_ft_trn.R")

dir_raw_pubmed <- here::here("data", "raw", "pubmed")
# dir_pubmed <- fs::dir_create(here::here("data", "processed", "pubmed"))
dir_trn <- fs::dir_create(here::here("data", "processed", "trn"))

trn_si_filepath <- fs::path(dir_trn, "trn-si.rds")
trn_abs_filepath <- fs::path(dir_trn, "trn-abstract.rds")
trn_ft_filepath <- fs::path(dir_trn, "trn-ft.rds")

# Extract TRNs from: PubMed secondary identifier, PubMed abstract, and PDF full-text
pubmed_xmls <- fs::dir_ls(dir_raw_pubmed)

# Extract trns from pubmed secondary identifier, or read in if already extracted
if (!fs::file_exists(trn_si_filepath)){
  si <-
    pubmed_xmls  |>
    purrr::map_dfr(extract_pubmed, datatype = "databanks", quiet = FALSE) |>
    ctregistries::mutate_trn_registry(accession_number) |>
    tidyr::drop_na(trn) |>
    select(pmid, registry, trn_detected = trn) |>
    distinct() |>
    group_by(pmid) |>
    mutate(n_detected = row_number()) |>
    ungroup() |>
    mutate(
      source = "secondary_id",
      trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
    )
  
  readr::write_rds(si, trn_si_filepath)
} else si <- readr::read_rds(trn_si_filepath)


# Extract trns from pubmed abstract, or read in if already extracted
if (!fs::file_exists(trn_abs_filepath)){
  abs <-
    pubmed_xmls |>
    purrr::map_dfr(extract_pubmed, datatype = "abstract", quiet = FALSE) |>
    ctregistries::mutate_trn_registry(abstract) |>
    tidyr::drop_na(trn) |>
    distinct(pmid, registry, trn_detected = trn) |>
    group_by(pmid) |>
    mutate(n_detected = row_number()) |>
    ungroup() |>
    mutate(
      source = "abstract",
      trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
    )
  
  readr::write_rds(abs, trn_abs_filepath)
} else abs <- readr::read_rds(trn_abs_filepath)

# Extract trns from fulltext, or read in if already extracted

dir_xml <- here::here("data", "raw", "fulltext", "xml")
ft_xmls <- fs::dir_ls(dir_xml)

if (!fs::file_exists(trn_ft_filepath)){
  
  ft_missing_pids <-
    ft_xmls |> 
    purrr::map_dfr(get_grobid_ft_trn) |> 
    rename(trn_detected = trn, n_detected = n) |> 
    mutate(
      source = "ft",
      trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
    )
  
  ft <- 
    ft_missing_pids |> 
    
    # Full text from dois are missing pmids, so replace from results
    filter(is.na(pmid)) |> 
    select(-pmid) |> 
    
    # Join in pmids from results_registrations
    left_join(distinct(results_registrations, pmid, doi), by = "doi") |> 
    
    # Add in full text with pmid-only (no doi)
    bind_rows(filter(ft_missing_pids, !is.na(pmid))) |> 
    relocate(pmid, .after = "doi")
  
  readr::write_rds(ft, trn_ft_filepath)
} else ft <- readr::read_rds(trn_ft_filepath)


# Combine links -----------------------------------------------------------

# Get list of full text pids (doi if available, else pmid)
ft_available <-
  ft_xmls |> 
  fs::path_file() |> 
  stringr::str_remove(".tei.xml$") |> 
  stringr::str_replace_all("\\+", "/")

direcct_links <-
  
  # Combine publication-based links (si, abs, ft, publication in registry)
  bind_rows(si, abs, ft) |>
  distinct(trn = trn_cleaned, pmid, source) |>
  
  # Since some analyses depend on pubmed data, drop missing pmids
  filter(!is.na(pmid)) |> 
  
  # Make trns practices into columns
  mutate(has_trn = TRUE) |> 
  tidyr::pivot_wider(id_cols = c(trn, pmid), names_from = source, names_prefix = "has_trn_", values_from = has_trn, values_fill = FALSE) |> 
  
  # Add publication-based links into registry-based links
  left_join(ref_links, y = _, by = c("trn", "pmid")) |> 
  
  # Some empty cells because no links, so replace NAs with FALSE
  # NOTE: Logically some are truly not available
  mutate(across(-c(id, trn, registry, doi, pmid), ~ tidyr::replace_na(., FALSE))) |> 
  
  # Apply critera
  #
  # Criteria already applied
  # * full results journal article
  # * publication has pmid (may or may not have doi)
  #
  # Criteria to apply:
  # * publication full-text available
  # * earliest publication (if >1 full result with pmid associated with trial)
  # * registry ctgov or drks
  # * best linked trn (if >1 trn associated with trial)
  
  # Flag whether full-text available
  mutate(
    has_ft = if_else(doi %in% ft_available | pmid %in% ft_available, TRUE, FALSE)
  ) |> 
  
  # Flag earlierst publicsation (important for trials with multiple publications)
  # Note: tri02586 has 2 publications (10.1056/nejmoa2103417 on critical and 10.1056/nejmoa2105911 on noncritical) on same date, so pick one of tie
  group_by(id, trn) |>
  arrange(date_publication, doi) |> # arrange by doi for consistency tie-ranking
  mutate(n_pub_ordered = row_number()) |> 
  ungroup() |> 
  mutate(is_earliest_pub = if_else(n_pub_ordered == 1, TRUE, FALSE)) |> 
  
  # Flag best linked TRN per trial 
  # Note: Per registry, so can filter for registries (no ctgov/drks crossreg)
  # DIRECCT captured sometimes more than 1 trn per trial. In `direcct_links`, 4 trials have >1 trn:
  # tri00985 (6 TRNs), tri03086 (3 TRNs), tri02344 (2 TRNs), tri02944 (2 TRNs)
  # direcct_links |> janitor::get_dupes(id)
  # Since we are interested in how well linked trials are (not how well linked individual trns are), we generously select the best linked trn per trial. For example, "tri02944" has "NCT04348656" with 3 links and "NCT04418518" with 0 links, so we keep "NCT04348656".
  group_by(id, trn, registry, doi, pmid) |> 
  mutate(n_links = sum(has_reg_pub_link,
                       has_trn_secondary_id,
                       has_trn_abstract,
                       has_trn_ft)) |> 
  ungroup(trn) |>
  arrange(desc(n_links), trn) |> # arrange by trn for consistency tie-ranking
  mutate(is_best_linked = if_else(row_number() == 1, TRUE, FALSE)) |> 
  ungroup()

readr::write_csv(direcct_links, here::here("data", "processed", "direcct-links-all.csv"))


# Apply inclusion criteria ------------------------------------------------
  
direcct_links_analysis <-
  direcct_links |> 
  
  filter(
    has_ft, 
    is_earliest_pub, 
    is_best_linked, 
    registry %in% c("ClinicalTrials.gov", "DRKS")
  )

readr::write_csv(direcct_links_analysis, here::here("data", "processed", "direcct-links-analysis.csv"))