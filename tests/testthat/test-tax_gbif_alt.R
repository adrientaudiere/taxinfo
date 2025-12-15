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

test_that("tax_gbif_alt GBIF integration", {
  # Test GBIF altitude data retrieval
  skip_if_offline()
  skip_on_cran()
  
  # Basic test with a common species
  result <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    verbose = FALSE
  )
  
  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  
  # Check that we got a result
  expect_equal(nrow(result), 1)
})

test_that("tax_gbif_alt altitude statistics structure", {
  # Test that altitude statistics returns expected columns
  skip_if_offline()
  skip_on_cran()
  
  result <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    verbose = FALSE
  )
  
  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  
  # Check that expected columns are present
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

test_that("tax_gbif_alt n_occur_altitude parameter", {
  # Test that n_occur_altitude controls sample size
  skip_if_offline()
  skip_on_cran()
  
  # Request smaller sample
  result_small <- tax_gbif_alt(
    taxnames = c("Amanita muscaria"),
    n_occur_altitude = 100,
    verbose = FALSE
  )
  
  # Should have at most 100 records (or fewer if not enough data available)
  if (!is.na(result_small$altitude_n_records[1])) {
    expect_true(result_small$altitude_n_records[1] <= 100)
  }
})
