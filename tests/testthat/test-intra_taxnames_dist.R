# Test intra_taxnames_dist function
# Examples from man page: intra_taxnames_dist.Rd

test_that("intra_taxnames_dist input validation", {
  # Test with NULL phyloseq object
  expect_error(intra_taxnames_dist(NULL))
})

# Examples from man page: intra_taxnames_dist.Rd (lines 44-47)
test_that("intra_taxnames_dist returns expected structure", {
  # Example: intra_taxn_dist <- intra_taxnames_dist(data_fungi_mini)
  intra_taxn_dist <- intra_taxnames_dist(data_fungi_mini)

  # Should return a data.frame
  expect_s3_class(intra_taxn_dist, "data.frame")

  # Should have expected columns
  expect_true("taxnames" %in% colnames(intra_taxn_dist))
  expect_true("n_taxa" %in% colnames(intra_taxn_dist))
  expect_true("mean_dist" %in% colnames(intra_taxn_dist))
  expect_true("min_dist" %in% colnames(intra_taxn_dist))
  expect_true("max_dist" %in% colnames(intra_taxn_dist))
})

test_that("intra_taxnames_dist distance values are valid", {
  # Example: intra_taxn_dist <- intra_taxnames_dist(data_fungi_mini)
  intra_taxn_dist <- intra_taxnames_dist(data_fungi_mini)

  # Distance values should be numeric where not NA
  expect_true(is.numeric(intra_taxn_dist$mean_dist))
  expect_true(is.numeric(intra_taxn_dist$min_dist))
  expect_true(is.numeric(intra_taxn_dist$max_dist))

  # n_taxa should be integer values >= 1
  expect_true(all(intra_taxn_dist$n_taxa >= 1))
})
