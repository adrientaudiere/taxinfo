# Test cluster_sbc function
# Examples from man page: cluster_sbc.Rd

test_that("cluster_sbc input validation", {
  # Test with NULL phyloseq object
  expect_error(cluster_sbc(NULL))
})

# Examples from man page: cluster_sbc.Rd (lines 93-115)
test_that("cluster_sbc returns expected structure", {
  # Example: res <- cluster_sbc(data_fungi_mini)
  res <- cluster_sbc(data_fungi_mini)

  # Should return a list
  expect_type(res, "list")

  # Should contain expected elements
  expect_true("clusters" %in% names(res))
  expect_true("summary" %in% names(res))
  expect_true("d_per_taxnames" %in% names(res))
  expect_true("physeq_with_info" %in% names(res))
  expect_true("physeq_SBC" %in% names(res))
})

test_that("cluster_sbc returns phyloseq objects", {
  # Example: res <- cluster_sbc(data_fungi_mini)
  res <- cluster_sbc(data_fungi_mini)

  # physeq_with_info should be a phyloseq object
  expect_s4_class(res$physeq_with_info, "phyloseq")

  # physeq_SBC should be a phyloseq object
  expect_s4_class(res$physeq_SBC, "phyloseq")
})

test_that("cluster_sbc summary has expected structure", {
  # Example: res <- cluster_sbc(data_fungi_mini)
  res <- cluster_sbc(data_fungi_mini)

  # Summary should be a data.frame
  expect_s3_class(res$summary, "data.frame")

  # d_per_taxnames should be a data.frame
  expect_s3_class(res$d_per_taxnames, "data.frame")
  expect_true("taxnames" %in% colnames(res$d_per_taxnames))
  expect_true("n_taxa" %in% colnames(res$d_per_taxnames))
  expect_true("optimal_d" %in% colnames(res$d_per_taxnames))
})
