# Test tax_get_wk_info_pq function
# Examples from man page: tax_get_wk_info_pq.Rd

test_that("tax_get_wk_info_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_get_wk_info_pq(NULL))
})

# Examples from vignette: getting-started.Rmd and man page: tax_get_wk_info_pq.Rd
test_that("tax_get_wk_info_pq with taxnames returns tibble", {
  # Example from vignette: wiki_data <- tax_get_wk_info_pq(taxnames = taxa_to_query)
  taxa_to_query <- c(
    "Amanita muscaria",
    "Boletus edulis",
    "Cantharellus cibarius"
  )
  wiki_data <- tax_get_wk_info_pq(taxnames = taxa_to_query)

  expect_s3_class(wiki_data, "tbl_df")
  expect_true(nrow(wiki_data) > 0)
})

test_that("tax_get_wk_info_pq returns phyloseq with add_to_phyloseq = TRUE", {
  # Example: data_fungi_mini_cleanNames_wk_info <- tax_get_wk_info_pq(data_fungi_mini_cleanNames)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  data_fungi_mini_cleanNames_wk_info <- tax_get_wk_info_pq(
    data_fungi_mini_cleanNames
  )

  expect_s4_class(data_fungi_mini_cleanNames_wk_info, "phyloseq")
})

test_that("tax_get_wk_info_pq returns tibble with add_to_phyloseq = FALSE", {
  # Example: wk_info <- tax_get_wk_info_pq(subset_taxa_pq(...))
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  wk_info <- tax_get_wk_info_pq(
    data_fungi_mini_cleanNames,
    add_to_phyloseq = FALSE
  )

  expect_s3_class(wk_info, "tbl_df")
})
