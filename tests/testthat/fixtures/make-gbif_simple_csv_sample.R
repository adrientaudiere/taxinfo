# Regenerate the SIMPLE_CSV download fixture used by test-gbif_download_fixture.R.
# Requires GBIF credentials (GBIF_USER, GBIF_PWD, GBIF_EMAIL) and network access.
# Run from the package root:  Rscript tests/testthat/fixtures/make-gbif_simple_csv_sample.R
suppressMessages(devtools::load_all(quiet = TRUE))

keys <- rgbif::name_backbone("Xylobolus subpileatus")$usageKey
raw <- gbif_download(
  rgbif::pred("taxonKey", keys),
  rgbif::pred("hasCoordinate", TRUE),
  rgbif::pred("hasGeospatialIssue", FALSE),
  verbose = TRUE
)

saveRDS(
  utils::head(as.data.frame(raw), 200),
  testthat::test_path("fixtures", "gbif_simple_csv_sample.rds")
)
