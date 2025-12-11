# Test taxonomic_rank_to_taxnames function
# Examples from man page: taxonomic_rank_to_taxnames.Rd

test_that("taxonomic_rank_to_taxnames input validation", {
  # Test with NULL phyloseq object
  expect_error(taxonomic_rank_to_taxnames(NULL))
})

# Examples from man page: taxonomic_rank_to_taxnames.Rd (lines 40-46)
test_that("taxonomic_rank_to_taxnames basic usage", {
  # Example: taxonomic_rank_to_taxnames(data_fungi_mini)
  result <- taxonomic_rank_to_taxnames(data_fungi_mini)

  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("taxonomic_rank_to_taxnames with discard_genus_alone = TRUE", {
  # Example: taxonomic_rank_to_taxnames(data_fungi_mini, discard_genus_alone = TRUE)
  result <- taxonomic_rank_to_taxnames(data_fungi_mini, discard_genus_alone = TRUE)

  expect_type(result, "character")
  # Should have fewer results when discarding genus alone
  result_all <- taxonomic_rank_to_taxnames(data_fungi_mini, discard_genus_alone = FALSE)
  expect_true(length(result) <= length(result_all))
})

test_that("taxonomic_rank_to_taxnames with discard_NA = TRUE", {
  # Example: taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE)
  result <- taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE)

  expect_type(result, "character")
  # Should not contain NA values in results
  expect_false(any(is.na(result)))
})

test_that("taxonomic_rank_to_taxnames with combined parameters", {
  # Example: taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE, discard_genus_alone = TRUE)
  result <- taxonomic_rank_to_taxnames(data_fungi_mini,
    discard_NA = TRUE,
    discard_genus_alone = TRUE
  )

  expect_type(result, "character")
  expect_false(any(is.na(result)))
})
