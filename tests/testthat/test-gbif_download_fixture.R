# Offline coverage of the occ_download path. `fixtures/gbif_simple_csv_sample.rds`
# is a real (small) GBIF SIMPLE_CSV download captured once for
# "Xylobolus subpileatus"; testing against it guards the column-schema
# assumptions of attribute_gbif_records() and the download branches without
# paying the (minutes-long, async) download cost on every run. Regenerate it
# with tests/testthat/fixtures/make-gbif_simple_csv_sample.R.
fixture_path <- testthat::test_path("fixtures", "gbif_simple_csv_sample.rds")

test_that("SIMPLE_CSV fixture exposes the columns the download path relies on", {
  skip_if_not(file.exists(fixture_path), "SIMPLE_CSV fixture not generated")
  occ <- readRDS(fixture_path)
  expect_true(all(
    c(
      "taxonKey",
      "speciesKey",
      "species",
      "genus",
      "decimalLongitude",
      "decimalLatitude",
      "countryCode",
      "year",
      "gbifID",
      "scientificName"
    ) %in%
      names(occ)
  ))
})

test_that("attribute_gbif_records attributes real SIMPLE_CSV records", {
  skip_if_not(file.exists(fixture_path), "SIMPLE_CSV fixture not generated")
  occ <- readRDS(fixture_path)
  sp_key <- as.integer(
    names(sort(table(occ$speciesKey), decreasing = TRUE))[1]
  )
  gbif_taxa <- tibble::tibble(
    usageKey = sp_key,
    canonicalName = "Xylobolus subpileatus",
    verbatim_name = "Xylobolus subpileatus"
  )
  res <- attribute_gbif_records(occ, gbif_taxa)
  expect_gt(nrow(res), 0)
  expect_true(all(c("taxon_name", "usageKey") %in% names(res)))
  expect_true(all(res$taxon_name == "Xylobolus subpileatus"))
})
