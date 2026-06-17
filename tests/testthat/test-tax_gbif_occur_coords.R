test_that("tax_gbif_occur_coords input validation", {
  expect_error(tax_gbif_occur_coords(NULL))
  expect_error(tax_gbif_occur_coords(character()))
  expect_error(tax_gbif_occur_coords(123))
  expect_error(tax_gbif_occur_coords(c("Amanita muscaria", "")))
  expect_error(tax_gbif_occur_coords("Amanita muscaria", n_occur = 0))
  expect_error(tax_gbif_occur_coords("Amanita muscaria", n_occur = -1))
})

test_that("tax_gbif_occur_coords returns expected structure", {
  skip_on_cran()
  vcr::use_cassette("occur_coords_structure", {
    res <- tax_gbif_occur_coords(
      "Fomes fomentarius",
      n_occur = 5,
      method = "search",
      verbose = FALSE,
      time_to_sleep = 0
    )
  })
  expect_s3_class(res, "tbl_df")
  expect_true(all(
    c("taxon_name", "usageKey", "decimalLongitude", "decimalLatitude") %in%
      names(res)
  ))
  expect_true(is.numeric(res$decimalLongitude))
  expect_true(is.numeric(res$decimalLatitude))
  expect_true(!is.null(attr(res, "missing_taxa")))
})

test_that("tax_gbif_occur_coords handles unmatched taxa", {
  skip_on_cran()
  vcr::use_cassette("occur_coords_unmatched", {
    res <- tax_gbif_occur_coords(
      c("Amanita muscaria", "NotARealTaxon ZzYy"),
      n_occur = 5,
      method = "search",
      verbose = FALSE,
      time_to_sleep = 0
    )
  })
  expect_true("NotARealTaxon ZzYy" %in% attr(res, "missing_taxa"))
})
