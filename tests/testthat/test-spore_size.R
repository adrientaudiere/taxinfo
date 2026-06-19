# Test spore_size functions
# Tests for extract_spores_mycodb and tax_spores_size_pq functions

test_that("extract_spores_mycodb returns valid structure", {
  skip_if_no_slow_tests()
  # Test with known species that should exist in mycoDB
  result <- extract_spores_mycodb("Amanita muscaria", verbose = FALSE)

  expect_type(result, "character")
  expect_true(length(result) == 1)
})

test_that("extract_spores_mycodb handles non-existent species", {
  skip_if_no_slow_tests()
  # Test with a species that definitely doesn't exist
  result <- extract_spores_mycodb("Nonexistent fungusname", verbose = FALSE)

  expect_type(result, "character")
  # Should return "Not in mycoDB" for non-existent species
  expect_true(result == "Not in mycoDB")
})

test_that("extract_spores_mycodb handles genus-only input", {
  skip_if_no_slow_tests()
  # Test with genus-only input (no species)
  result <- extract_spores_mycodb("Amanita", verbose = FALSE)

  expect_type(result, "character")
  expect_true(length(result) == 1)
})

test_that("tax_spores_size_pq input validation", {
  # Test with NULL parameters
  expect_error(
    tax_spores_size_pq(physeq = NULL, taxnames = NULL),
    "You must specify either"
  )

  # Test that providing both physeq and taxnames causes error
  expect_error(
    tax_spores_size_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("tax_spores_size_pq add_to_phyloseq validation", {
  # Test that add_to_phyloseq = TRUE with taxnames causes error
  expect_error(
    tax_spores_size_pq(
      taxnames = c("Amanita muscaria"),
      add_to_phyloseq = TRUE
    ),
    "cannot be TRUE when.*taxnames"
  )
})

# Integration test with taxnames parameter
test_that("tax_spores_size_pq with taxnames returns dataframe", {
  skip_if_no_slow_tests()
  # Test with a known species
  result <- tax_spores_size_pq(
    taxnames = c("Boletus edulis"),
    verbose = FALSE,
    time_to_sleep = 0
  )

  expect_s3_class(result, "data.frame")
  expect_true("spore_size" %in% colnames(result))
  expect_true("spore_length_mean" %in% colnames(result))
  expect_true("spore_width_mean" %in% colnames(result))
  expect_true("taxa_name" %in% colnames(result))
})

test_that("extract_spores_mycodb verbose parameter works", {
  skip_if_no_slow_tests()
  # Test with verbose = TRUE (should not error)
  expect_no_error(extract_spores_mycodb("Boletus edulis", verbose = TRUE))

  # Test with verbose = FALSE
  expect_no_error(extract_spores_mycodb("Boletus edulis", verbose = FALSE))
})
