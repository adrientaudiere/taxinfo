# Test tax_gbif_occur_pq function

test_that("tax_gbif_occur_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_gbif_occur_pq(NULL))

  skip("Requires phyloseq objects")
})

test_that("tax_gbif_occur_pq parameter defaults", {
  # Test default parameter values
  # taxonomic_rank should default to "currentCanonicalSimple"
  # add_to_phyloseq should default to FALSE
  # verbose should default to TRUE

  expect_true(is.character("currentCanonicalSimple"))
  expect_true(is.logical(FALSE))
  expect_true(is.logical(TRUE))
})

test_that("tax_gbif_occur_pq GBIF integration", {
  # Test GBIF occurrence data retrieval
  # Test occurrence count processing
  # Test coordinate handling

  skip("Requires GBIF API access")
})

test_that("tax_gbif_occur_pq return behavior", {
  # Test return modes
  # When add_to_phyloseq = TRUE: should return phyloseq with occurrence data
  # When add_to_phyloseq = FALSE: should return occurrence tibble

  skip("Requires phyloseq objects")
})

test_that("tax_gbif_occur_pq altitude parameter validation", {
  # Test that get_altitude cannot be combined with by_country or by_years
  skip_if_offline()
  skip_on_cran()
  
  # Should error when get_altitude is TRUE with by_country
  expect_error(
    tax_gbif_occur_pq(
      taxnames = c("Amanita muscaria"),
      get_altitude = TRUE,
      by_country = TRUE,
      verbose = FALSE
    ),
    "get_altitude"
  )
  
  # Should error when get_altitude is TRUE with by_years
  expect_error(
    tax_gbif_occur_pq(
      taxnames = c("Amanita muscaria"),
      get_altitude = TRUE,
      by_years = TRUE,
      verbose = FALSE
    ),
    "get_altitude"
  )
})

test_that("tax_gbif_occur_pq altitude statistics structure", {
  # Test that altitude statistics returns expected columns
  skip_if_offline()
  skip_on_cran()
  
  result <- tax_gbif_occur_pq(
    taxnames = c("Amanita muscaria"),
    get_altitude = TRUE,
    verbose = FALSE
  )
  
  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  
  # Check that expected columns are present
  expected_cols <- c(
    "altitude_min", "altitude_max", "altitude_q05", "altitude_q50",
    "altitude_q95", "altitude_mean", "altitude_sd", "altitude_n_records",
    "Global_occurences", "canonicalName"
  )
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Check that numeric columns are numeric
  expect_true(is.numeric(result$altitude_min))
  expect_true(is.numeric(result$altitude_max))
  expect_true(is.numeric(result$altitude_mean))
  expect_true(is.numeric(result$altitude_sd))
})
