# Download file to `dir` within "data" directory, if not already downloaded
download_file <- function(file_url, file_name, dir = "raw"){
  dir_path <- fs::dir_create(fs::path_wd("data", dir))
  file_path <- fs::path(dir_path, file_name)
  if (!fs::file_exists(file_path)) {
    # Simply download CSV; Convert RDS to CSV before saving
    if (stringr::str_detect(file_url, "\\.rds")) {
      readr::read_rds(file_url) |>
        readr::write_csv(file_path)
    } else {download.file(file_url, file_path)}
  }
}

download_file("https://zenodo.org/records/8181415/files/results.csv?download=1", "results.csv")
download_file("https://zenodo.org/records/8181415/files/registrations.csv?download=1", "registrations.csv")
