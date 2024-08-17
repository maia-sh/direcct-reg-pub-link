library(dplyr)

source("https://github.com/maia-sh/intovalue-data/raw/main/scripts/functions/download_pubmed.R")

dir_raw_pubmed <- fs::dir_create(here::here("data", "raw", "pubmed"))

results <- readr::read_csv(here::here("data", "processed", "results.csv"))

# Get pmids from results LIMITED TO "full_results_journal_article"
pmids <-
  results |> 
  filter(pub_type == "full_results_journal_article") |> 
  filter(!is.na(pmid)) |> 
  distinct(pmid) |> 
  arrange(pmid) |> 
  pull()

# If pmids already downloaded, remove those from list to download
if (fs::dir_exists(dir_raw_pubmed)){
  
  pmids_downloaded <-
    fs::dir_ls(dir_raw_pubmed) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    as.numeric()
  
  # Check whether pmids downloaded which aren't needed and manually review and remove
  pmids_downloaded_unused <- setdiff(pmids_downloaded, pmids)
  if (length(pmids_downloaded_unused) > 0) {
    rlang::warn(glue::glue("Unused pmid downloaded: {pmids_downloaded_unused}"))
  }
  
  pmids <- setdiff(pmids, pmids_downloaded)
}

# Download remaining pmids, if any
if (length(pmids) > 0) {
  
  # Use pubmed api key locally stored as "ncbi-pubmed", if available
  # Else ask user and store
  pubmed_api_key <-
    ifelse(
      nrow(keyring::key_list("ncbi-pubmed")) == 1,
      keyring::key_get("ncbi-pubmed"),
      keyring::key_set("ncbi-pubmed")
    )
  
  pmids %>%
    purrr::walk(download_pubmed,
                dir = dir_raw_pubmed,
                api_key = pubmed_api_key
    )
  
  # Log query date
  loggit::set_logfile(here::here("data", "queries.log"))
  loggit::loggit("INFO", "PubMed")
}
