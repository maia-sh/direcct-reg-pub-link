library(dplyr)
library(aactr)

results <- readr::read_csv(here::here("data", "processed", "results.csv"))
registrations <- readr::read_csv(here::here("data", "raw", "registrations.csv"))

dir_ctgov_raw <- fs::dir_create(here::here("data", "raw", "registries", "ctgov"))
dir_ctgov_processed <- fs::dir_create(here::here("data", "processed", "registries", "ctgov"))

trns <-
  results |> 
  
  # Limit to full results
  filter(pub_type == "full_results_journal_article") |>
  
  # Get all registrations
  semi_join(registrations, y = _, by = "id")


# ClinicalTrials.gov ------------------------------------------------------

trns_ctgov <-
  trns |> 
  filter(registry == "ClinicalTrials.gov") |> 
  pull(trn)

# Specify aact username
AACT_USER <- "respmetrics"

# Download and process aact data if not already done
if (length(fs::dir_ls(dir_ctgov_raw)) == 0){
  message("Downloading AACT")
  download_aact(ids = trns_ctgov, dir = dir_ctgov_raw, user = AACT_USER)
}

if (length(fs::dir_ls(dir_ctgov_processed)) == 0){
  message("Processing AACT")
  process_aact(dir_ctgov_raw, dir_ctgov_processed)
}


# DRKS --------------------------------------------------------------------
# devtools::install_github("quest-bih/dRks")
library(dRks)

dir_drks_raw <- fs::dir_create(here::here("data", "raw", "registries", "drks"))

trns_drks <-
  trns |> 
  filter(registry == "DRKS") |> 
  pull(trn)

# Note: Only 1 registration in DRKS

# Download drks records
purrr::walk(trns_drks, dRks::download_drks, dir = dir_drks_raw)

# Log query date
loggit::set_logfile(here::here("queries.log"))
loggit::loggit("INFO", "DRKS")
