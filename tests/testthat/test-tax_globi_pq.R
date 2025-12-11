# Test tax_globi_pq function
# Examples from man page: tax_globi_pq.Rd

test_that("tax_globi_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_globi_pq(NULL))
})

# Examples from man page: tax_globi_pq.Rd (lines 91-106)
test_that("tax_globi_pq returns tibble with add_to_phyloseq = FALSE", {
  # Example: res_globi <- tax_globi_pq(data_fungi_mini,
  #   taxonomic_rank = c("Genus", "Species"),
  #   interaction_types = list("parasiteOf", "hasHost"),
  #   verbose = TRUE, max_interactions = 10)
  res_globi <- tax_globi_pq(data_fungi_mini,
    taxonomic_rank = c("Genus", "Species"),
    interaction_types = list("parasiteOf", "hasHost"),
    verbose = TRUE,
    max_interactions = 10,
    add_to_phyloseq = FALSE
  )

  expect_s3_class(res_globi, "tbl_df")
})

test_that("tax_globi_pq returns phyloseq with add_to_phyloseq = TRUE", {
  # Example: data_fungi_mini_cleanNames <- tax_globi_pq(data_fungi_mini_cleanNames,
  #   interaction_types = c("hasHost"))
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
  result <- tax_globi_pq(data_fungi_mini_cleanNames,
    interaction_types = c("hasHost")
  )

  expect_s4_class(result, "phyloseq")
})
