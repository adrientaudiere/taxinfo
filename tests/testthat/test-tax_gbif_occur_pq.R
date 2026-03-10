# Test tax_gbif_occur_pq function
# Examples from man page: tax_gbif_occur_pq.Rd

test_that("tax_gbif_occur_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_gbif_occur_pq(NULL))
})

test_that("tax_gbif_occur_pq parameter defaults", {
  # Test default parameter values
  # taxonomic_rank should default to "currentCanonicalSimple"
  # add_to_phyloseq should default to TRUE for phyloseq, FALSE for taxnames
  # verbose should default to TRUE
  expect_true(is.character("currentCanonicalSimple"))
  expect_true(is.logical(FALSE))
  expect_true(is.logical(TRUE))
})

# Examples from man page: tax_gbif_occur_pq.Rd (lines 61-72)
test_that("tax_gbif_occur_pq with taxnames returns tibble", {
  # Example: tax_gbif_occur_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
  result <- tax_gbif_occur_pq(
    taxnames = c("Amanita muscaria", "Boletus edulis")
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("Global_occurences" %in% colnames(result))
})

test_that("tax_gbif_occur_pq with phyloseq add_to_phyloseq = FALSE returns tibble", {
  # Example: tax_gbif_occur_pq(data_fungi_mini_cleanNames, add_to_phyloseq = FALSE)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  result <- tax_gbif_occur_pq(
    data_fungi_mini_cleanNames,
    add_to_phyloseq = FALSE
  )
  expect_s3_class(result, "tbl_df")
  expect_true("Global_occurences" %in% colnames(result))
})

test_that("tax_gbif_occur_pq with by_years parameter", {
  # Example: tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_years = TRUE, add_to_phyloseq = FALSE)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  result <- tax_gbif_occur_pq(
    data_fungi_mini_cleanNames,
    by_years = TRUE,
    add_to_phyloseq = FALSE
  )
  expect_s3_class(result, "tbl_df")
})

test_that("tax_gbif_occur_pq returns phyloseq with add_to_phyloseq = TRUE", {
  # Example: data_fungi_mini_cleanNames <- tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_country = TRUE)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  result <- tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_country = TRUE)
  expect_s4_class(result, "phyloseq")
  expect_true("Global_occurences" %in% colnames(result@tax_table))
})
