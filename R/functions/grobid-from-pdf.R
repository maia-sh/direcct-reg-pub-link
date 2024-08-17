# Convert pdf to xml using QUEST-server instance of GROBID (also available at https://quest-grobid.charite.de/)
# Need connection to VPN
# Credit: Vladislav Nachev

grobid_from_pdf <- function(filepath, destination_folder = NULL) {

  stopifnot(stringr::str_detect(filepath, ".pdf$|.PDF$"))
  
  if (is.null(destination_folder)) destination_folder <- fs::path_dir(filepath)
  
  dest_filename <- fs::path_file(filepath) |> fs::path_ext_set(".tei.xml") 
  dest_filename <- fs::path(destination_folder, dest_filename)
  
  if (fs::file_exists(dest_filename)) {
    print("File already processed.")
    return()
  }
  
  req <- httr2::request("s-appserv5.bihealth.org:8070/api/processFulltextDocument")
  tryCatch(
    {
      resp <- req |> 
        httr2::req_body_multipart(input = curl::form_file(filepath))  |> 
        httr2::req_perform(verbosity = 3) 
    },
    error = function(e) e, finally = print(paste("Processing", filepath))
  )
  
  res_j <- resp$body
  
  writeBin(res_j, dest_filename)
  print("Success")
}