# Test tax_iucn_code_pq function
# Examples from man page: tax_iucn_code_pq.Rd

test_that("tax_iucn_code_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_iucn_code_pq(NULL))
})

# Examples from man page: tax_iucn_code_pq.Rd (lines 45-53)
test_that("tax_iucn_code_pq returns phyloseq with iucn_code column", {
  skip_if_offline()
  skip_on_cran()
  # Example: data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini) |>
  #   tax_iucn_code_pq()
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini) |>
    tax_iucn_code_pq()

  expect_s4_class(data_fungi_mini_cleanNames, "phyloseq")
  expect_true("iucn_code" %in% colnames(data_fungi_mini_cleanNames@tax_table))
})

test_that("tax_iucn_code_pq with taxnames returns tibble", {
  skip_if_offline()
  skip_on_cran()
  # Example: tax_iucn_code_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
  result <- tax_iucn_code_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("iucn_code" %in% colnames(result))
})
