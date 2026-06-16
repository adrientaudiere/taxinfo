skip_if_no_gbif_credentials <- function() {
  has_creds <-
    Sys.getenv("GBIF_USER") != "" &&
      Sys.getenv("GBIF_PWD") != "" &&
      Sys.getenv("GBIF_EMAIL") != ""
  testthat::skip_if(!has_creds, "GBIF credentials not available")
}
