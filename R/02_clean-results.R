library(dplyr)

results <- 
  readr::read_csv(here::here("data", "raw", "results.csv")) |> 
  
  # Convert dois to lowercase (just in case!)
  mutate(doi = stringr::str_to_lower(doi)) |> 
  
  # Fix a doi
  mutate(doi = if_else(doi == "10.3389/fmed.2021.691712/full", "10.3389/fmed.2021.691712", doi, missing = doi)) |> 
  
  # Fix pmid
  mutate(pmid = if_else(doi == "10.1002/advs.202001435", 35403380, pmid, missing = pmid)) |> 
  
  # Add missing doi and pmid
  rows_patch(
    tibble(doi = "10.1007/s00784-020-03549-1", pmid = 32876748, url = "https://link.springer.com/article/10.1007%2Fs00784-020-03549-1"),
    by = "url"
  )


# Get missing dois and pmids, if available --------------------------------

source("https://github.com/maia-sh/pubmedparser/raw/main/R/fatcat_convert.R")

pmids_to_dois <-
  results |> 
  filter(is.na(doi), !is.na(pmid)) |> 
  distinct(pmid) |> 
  pull(pmid) |> 
  purrr::map_dfr(fatcat_convert, type = "pmid") |> 
  mutate(pmid = as.numeric(pmid))

# Note: This is slow (~2min, many individual queries)
dois_to_pmids <-
  results |> 
  filter(!is.na(doi), is.na(pmid)) |> 
  distinct(doi) |> 
  pull(doi) |> 
  purrr::map_dfr(fatcat_convert, type = "doi") |> 
  mutate(pmid = as.numeric(pmid))

# Add dois/pmids to results
results_ids <-
  results |> 
  rows_update(pmids_to_dois, by = "pmid") |> 
  rows_update(dois_to_pmids, by = "doi") |> 
  
  # Fix pmid, incorrect in fatcat
  mutate(pmid = if_else(doi == "10.1016/j.ijrobp.2021.07.433", NA, pmid, missing = pmid))
  
readr::write_csv(results_ids, here::here(fs::dir_create("data", "processed"), "results.csv"))
