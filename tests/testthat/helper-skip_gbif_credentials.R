skip_if_no_gbif_credentials <- function() {
  testthat::skip_if(
    !has_gbif_credentials(),
    "GBIF credentials not available"
  )
}
