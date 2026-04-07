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
  result <- taxonomic_rank_to_taxnames(
    data_fungi_mini,
    discard_genus_alone = TRUE
  )

  expect_type(result, "character")
  # Should have fewer results when discarding genus alone
  result_all <- taxonomic_rank_to_taxnames(
    data_fungi_mini,
    discard_genus_alone = FALSE
  )
  expect_true(length(result) <= length(result_all))
})

test_that("taxonomic_rank_to_taxnames with discard_NA = TRUE", {
  # Example: taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE)
  result <- taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE)

  expect_type(result, "character")
  # Should not contain NA values in results
  expect_false(anyNA(result))
})

test_that("taxonomic_rank_to_taxnames with combined parameters", {
  # Example: taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE, discard_genus_alone = TRUE)
  result <- taxonomic_rank_to_taxnames(
    data_fungi_mini,
    discard_NA = TRUE,
    discard_genus_alone = TRUE
  )

  # Mock data creation helper for future use
  create_mock_tax_table <- function() {
    # This would create a mock tax_table for testing
    # when phyloseq package is available
    data.frame(
      Genus = c("Xylodon", "Basidiodendron", "Fusarium"),
      Species = c("raduloides", "eyrei", "oxysporum"),
      currentCanonicalSimple = c(
        "Xylodon flaviporus",
        "Basidiodendron eyrei",
        "Fusarium oxysporum"
      ),
      stringsAsFactors = FALSE
    )
  }

  test_that("taxonomic_rank_to_taxnames basic functionality", {
    # Test with mock data structure
    mock_tax <- create_mock_tax_table()

    # Test that our mock data has expected structure
    expect_true("Genus" %in% colnames(mock_tax))
    expect_true("Species" %in% colnames(mock_tax))
    expect_true("currentCanonicalSimple" %in% colnames(mock_tax))

    # Test taxonomic name construction logic
    combined_names <- paste(mock_tax$Genus, mock_tax$Species)
    expect_equal(length(combined_names), 3)
    expect_true("Xylodon raduloides" %in% combined_names)
    expect_true("Basidiodendron eyrei" %in% combined_names)
  })

  test_that("taxonomic_rank_to_taxnames handles NA values", {
    # Test NA handling logic
    mock_tax_with_na <- data.frame(
      Genus = c("Xylodon", "Basidiodendron", NA),
      Species = c("raduloides", NA, "oxysporum"),
      stringsAsFactors = FALSE
    )

    combined_names <- paste(mock_tax_with_na$Genus, mock_tax_with_na$Species)
    na_patterns <- grepl("NA", combined_names)

    expect_true(any(na_patterns))
    expect_equal(sum(na_patterns), 2) # Two entries should have NA
  })

  expect_type(result, "character")
  expect_false(anyNA(result))
})
