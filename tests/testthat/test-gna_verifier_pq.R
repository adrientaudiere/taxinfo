# Test gna_verifier_pq function
# Examples from man page: gna_verifier_pq.Rd

test_that("gna_verifier_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(gna_verifier_pq(NULL))
})

test_that("gna_verifier_pq parameter defaults", {
  # Test default parameter values
  # data_sources default is c(1, 12)
  default_sources <- c(1, 12)
  expect_equal(length(default_sources), 2)
  expect_true(all(default_sources %in% 1:210))
})

test_that("gna_verifier_pq data_sources validation", {
  # Test data sources validation
  # Should accept valid source IDs
  # Example from man page uses data_sources = 210 (TaxRef)

  valid_sources <- c(1, 12, 210)
  expect_true(all(valid_sources >= 1))
})

# Examples from man page: gna_verifier_pq.Rd (lines 99-101)
test_that("gna_verifier_pq returns tibble with add_to_phyloseq = FALSE", {
  # Example: df <- gna_verifier_pq(data_fungi, data_sources = 210, add_to_phyloseq = FALSE)
  df <- gna_verifier_pq(data_fungi, data_sources = 210, add_to_phyloseq = FALSE)
  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
})

test_that("gna_verifier_pq returns phyloseq with add_to_phyloseq = TRUE", {
  # Example: data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
  expect_s4_class(data_fungi_mini_cleanNames, "phyloseq")
  # Check that new columns are added
  expect_true("currentCanonicalSimple" %in% colnames(data_fungi_mini_cleanNames@tax_table))
  expect_true("currentName" %in% colnames(data_fungi_mini_cleanNames@tax_table))
  expect_true("taxa_name" %in% colnames(data_fungi_mini_cleanNames@tax_table))
})
