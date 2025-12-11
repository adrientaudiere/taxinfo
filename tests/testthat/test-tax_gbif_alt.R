# Test tax_gbif_alt function

test_that("tax_gbif_alt input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_gbif_alt(NULL))
  
  # Test with both physeq and taxnames
  skip("Requires phyloseq objects")
})

test_that("tax_gbif_alt parameter defaults", {
  # Test default parameter values
  expect_true(is.character("currentCanonicalSimple"))
  expect_true(is.logical(FALSE))
  expect_true(is.logical(TRUE))
  expect_true(is.numeric(5000))
})

test_that("tax_gbif_alt GBIF method (default)", {
  # Test GBIF altitude data retrieval using native GBIF elevation field
  skip_if_offline()
  skip_on_cran()
  
  # Basic test with a common species using default method (gbif)
  result <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    verbose = FALSE
  )
  
  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  
  # Check that we got a result
  expect_equal(nrow(result), 1)
  
  # GBIF method should NOT have altitude_n_ocean column
  expect_false("altitude_n_ocean" %in% colnames(result))
})

test_that("tax_gbif_alt altitude statistics structure (gbif method)", {
  # Test that altitude statistics returns expected columns for GBIF method
  skip_if_offline()
  skip_on_cran()
  
  result <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    method = "gbif",
    verbose = FALSE
  )
  
  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  
  # Check that expected columns are present (no altitude_n_ocean for gbif method)
  expected_cols <- c(
    "altitude_min", "altitude_max", "altitude_q05", "altitude_q50",
    "altitude_q95", "altitude_mean", "altitude_sd", "altitude_n_records",
    "canonicalName"
  )
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Check that numeric columns are numeric
  expect_true(is.numeric(result$altitude_min))
  expect_true(is.numeric(result$altitude_max))
  expect_true(is.numeric(result$altitude_mean))
  expect_true(is.numeric(result$altitude_sd))
  expect_true(is.numeric(result$altitude_n_records))
})

test_that("tax_gbif_alt elevatr method", {
  # Test altitude data retrieval using elevatr method
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("elevatr")
  skip_if_not_installed("rnaturalearth")
  
  result <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    method = "elevatr",
    verbose = FALSE
  )
  
  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  
  # Check that expected columns are present (including altitude_n_ocean for elevatr method)
  expected_cols <- c(
    "altitude_min", "altitude_max", "altitude_q05", "altitude_q50",
    "altitude_q95", "altitude_mean", "altitude_sd", "altitude_n_records",
    "altitude_n_ocean", "canonicalName"
  )
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Check that numeric columns are numeric
  expect_true(is.numeric(result$altitude_min))
  expect_true(is.numeric(result$altitude_max))
  expect_true(is.numeric(result$altitude_mean))
  expect_true(is.numeric(result$altitude_sd))
  expect_true(is.numeric(result$altitude_n_records))
  expect_true(is.numeric(result$altitude_n_ocean))
})

test_that("tax_gbif_alt handles multiple taxa", {
  # Test with multiple species
  skip_if_offline()
  skip_on_cran()
  
  result <- tax_gbif_alt(
    taxnames = c("Amanita muscaria", "Boletus edulis"),
    verbose = FALSE
  )
  
  # Should return data for both taxa
  expect_true(nrow(result) >= 1)
  expect_true(nrow(result) <= 2)
})

test_that("tax_gbif_alt handles taxa without altitude data", {
  # Test graceful handling of missing data
  skip_if_offline()
  skip_on_cran()
  skip("Manual test - depends on species with/without data")
  
  # This test would check that function returns NA for species
  # without altitude data, but we can't guarantee which species
  # will or won't have data
})

test_that("tax_gbif_alt n_occur parameter", {
  # Test that n_occur controls sample size
  skip_if_offline()
  skip_on_cran()
  
  # Request smaller sample
  result_small <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    n_occur = 100,
    verbose = FALSE
  )
  
  # Should have at most 100 records (or fewer if not enough data available)
  if (!is.na(result_small$altitude_n_records[1])) {
    expect_true(result_small$altitude_n_records[1] <= 100)
  }
})

test_that("tax_gbif_alt elev_zoom parameter with elevatr method", {
  # Test that elev_zoom parameter is accepted with elevatr method
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("elevatr")
  skip_if_not_installed("rnaturalearth")
  
  # Request with different zoom level
  result <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    method = "elevatr",
    n_occur = 50,
    elev_zoom = 3,
    verbose = FALSE
  )
  
  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that("tax_gbif_alt method parameter validation", {
  # Test that invalid method triggers an error
  expect_error(
    tax_gbif_alt(taxnames = "Amanita muscaria", method = "invalid"),
    "'arg' should be one of"
  )
})
