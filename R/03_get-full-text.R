# devtools::install_github("ropensci-archive/rplos")
# devtools::install_github("ropensci-archive/microdemic")
# devtools::install_github("ropensci-archive/fulltext")
# devtools::install_github("ropensci-archive/crminer")
# devtools::install_github("quest-bih/pdfRetrieve") # quest private repo
library(fulltext)
library(pdfRetrieve)
library(dplyr)
library(fs)

dir_pdf <- fs::dir_create(here::here("data", "raw", "fulltext", "pdf"))

results <- readr::read_csv(here::here("data", "processed", "results.csv"))


# Prepare dois ------------------------------------------------------------

# Get dois from results LIMITED TO "full_results_journal_article"
dois_results <-
  results |> 
  filter(pub_type == "full_results_journal_article") |> 
  filter(!is.na(doi)) |> 
  distinct(doi) |> 
  arrange(doi) |> 
  pull()

# If dois already downloaded, remove those from list to download
pdfs_downloaded_already <-
  dir_ls(dir_pdf) |> 
  path_file() |> 
  path_ext_remove() |> 
  stringr::str_replace_all("\\+", "/")


# Download PDFs -----------------------------------------------------------

# Use email locally stored as "rm-email", if available
# Else ask user and store
email <-
  ifelse(
    nrow(keyring::key_list("rm-email")) == 1,
    keyring::key_get("rm-email"),
    keyring::key_set("rm-email")
  )

try(pdfRetrieve::pdf_retrieve(
  dois = setdiff(dois_results, pdfs_downloaded_already),
  email = email, 
  save_folder = dir_pdf
))


# Convert PDF dois to lowercase -------------------------------------------
# Even though input dois lowercase, some pdfs returned with uppercase dois
# Convert any uppercase for consistency and to facilitate comparisons

pdfs_downloaded_upper <-
  dir_ls(dir_pdf) |> 
  path_file() |> 
  stringr::str_subset("[[:upper:]]")

pdfs_downloaded_lower <- tolower(pdfs_downloaded_upper)

fs::file_move(path(dir_pdf, pdfs_downloaded_upper), path(dir_pdf, pdfs_downloaded_lower))


# Inform about pdf download success/failure -------------------------------

inform_pdf_download <- function(dir_pdf, dois_all, pdfs_downloaded_already){
  
  pdfs_downloaded <-
    dir_ls(dir_pdf) |> 
    path_file() |> 
    path_ext_remove() |> 
    stringr::str_replace_all("\\+", "/")
  
  pdfs_already <- intersect(dois_all, pdfs_downloaded_already)
  pdfs_searched <- setdiff(dois_all, pdfs_downloaded_already)
  pdfs_success <- intersect(pdfs_downloaded, pdfs_searched)
  pdfs_failure <- setdiff(pdfs_searched, pdfs_downloaded)
  pdfs_unused <- setdiff(pdfs_downloaded_already, dois_all)
  
  cli::cli_alert_info(cli::col_cyan("Previously downloaded {length(pdfs_already)} PDF{?s}"))
  cli::cli_alert_success(cli::col_green("Downloaded {length(pdfs_success)} PDF{?s}"))
  cli::cli_li(cli::col_blue(pdfs_success))
  cli::cli_alert_danger(cli::col_red("Failed to download {length(pdfs_failure)} PDF{?s}"))
  cli::cli_li(cli::col_blue(pdfs_failure))
  cli::cli_alert_warning(cli::col_yellow("Downloads include {length(pdfs_unused)} unused PDF{?s}"))
  cli::cli_li(cli::col_blue(pdfs_unused))
}

inform_pdf_download(dir_pdf, dois_results, pdfs_downloaded_already)

# Notes on PDFs -----------------------------------------------------------
# I manually reviewed PDFs to check that they were journal articles with appropriate title pages. I removed additional cover pages (e.g., Elsevier COVID notice, university cover pages, etc.).
# 
# I manually downloaded PDFs for few publications with pmid-only (no doi).
# 
# I also note some anomalies.
# 
# Not accessible:
# 10.2174/1568026621666210429083050 (accessed preprint 10.1101/2020.04.11.20061473)
# 10.1111/jocn.16171
# 
# Non-English:
# 10.20953/1729-9225-2020-3-30-40 (Russian)
# 10.3760/cma.j.cn121430-20200528-00485 (Chinese)
# 10.3760/cma.j.cn121430-20200810-00568 (Chinese)
# 10.3760/cma.j.cn121430-20210514-00714 (Chinese)
# 10.3785/j.issn.1008-9292.2020.03.03 (Chinese)
# 10.16333/j.1001-6880.2021.3.017 (Chinese)


# Convert PDF to XML ------------------------------------------------------
# Convert PDF to XML using GROBID on QUEST-server
# Must be connected to VPN
# Skip PDF if already converted

source(here::here("R", "functions", "grobid-from-pdf.R"))

dir_xml <- fs::dir_create(here::here("data", "raw", "fulltext", "xml"))

purrr::walk(dir_ls(dir_pdf), grobid_from_pdf, destination_folder = dir_xml)


# Inform about xml conversion success/failure -----------------------------

inform_pdf_xml_conversion <- function(dir_pdf, dir_xml){
  
  pdfs_downloaded <-
    dir_ls(dir_pdf) |> 
    path_file() |> 
    path_ext_remove() |> 
    stringr::str_replace_all("\\+", "/")
  
  xmls_converted <-
    dir_ls(dir_xml) |> 
    path_file() |> 
    stringr::str_remove(".tei.xml$") |> 
    stringr::str_replace_all("\\+", "/")
  
  xmls_success <- intersect(xmls_converted, pdfs_downloaded)
  xmls_failure <- setdiff(pdfs_downloaded, xmls_converted)
  
  cli::cli_alert_success(cli::col_green("Converted {length(xmls_success)} PDF{?s} to XML"))
  cli::cli_alert_danger(cli::col_red("Failed to convert {length(xmls_failure)} PDF{?s} to XML"))
  cli::cli_li(cli::col_blue(xmls_failure))
}


# Convert pmid-only pdfs to xml -------------------------------------------

dir_pdf_pmid <- here::here("data", "raw", "fulltext", "pdf-pmid-only")
purrr::walk(dir_ls(dir_pdf_pmid), grobid_from_pdf, destination_folder = dir_xml)
inform_pdf_xml_conversion(dir_pdf_pmid, dir_xml)

