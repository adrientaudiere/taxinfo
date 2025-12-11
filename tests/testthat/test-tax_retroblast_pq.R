# Test tax_retroblast_pq function
# Examples from man page: tax_retroblast_pq.Rd

test_that("tax_retroblast_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_retroblast_pq(NULL))
})

# Examples from man page: tax_retroblast_pq.Rd (lines 114-144)
test_that("tax_retroblast_pq returns list with add_to_phyloseq = FALSE", {
  # Example: res_retro <- tax_retroblast_pq(data_fungi_mini_cleanNames,
  #   marker = c("ITS", "internal transcribed spacer"),
  #   retmax = 10, id_cut = 99, add_to_phyloseq = FALSE)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
  res_retro <- tax_retroblast_pq(data_fungi_mini_cleanNames,
    marker = c("ITS", "internal transcribed spacer"),
    retmax = 10, id_cut = 99,
    add_to_phyloseq = FALSE
  )

  # Should return a list
  expect_type(res_retro, "list")
  expect_true("tib_retroblast" %in% names(res_retro))
  expect_true("entrez_search" %in% names(res_retro))

  # tib_retroblast should be a tibble
  expect_s3_class(res_retro$tib_retroblast, "tbl_df")
})

test_that("tax_retroblast_pq returns phyloseq with add_to_phyloseq = TRUE", {
  # Example: res_retro <- tax_retroblast_pq(data_fungi_mini_cleanNames,
  #   marker = c("ITS", "internal transcribed spacer"),
  #   retmax = 10, id_cut = 99)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
  res_retro <- tax_retroblast_pq(data_fungi_mini_cleanNames,
    marker = c("ITS", "internal transcribed spacer"),
    retmax = 10, id_cut = 99
  )

  expect_s4_class(res_retro, "phyloseq")
})
