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

test_that("tax_retroblast_pq tib_retroblast has expected columns", {
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
  res_retro <- tax_retroblast_pq(data_fungi_mini_cleanNames,
    marker = c("ITS", "internal transcribed spacer"),
    retmax = 10, id_cut = 99,
    add_to_phyloseq = FALSE
  )

  # Check for expected columns in tib_retroblast
  expected_cols <- c("taxa_name", "taxnames_species", "blast_queried", "blast_result", "good_assign")
  for (col in expected_cols) {
    expect_true(col %in% colnames(res_retro$tib_retroblast),
      info = paste("Missing column:", col)
    )
  }
})

test_that("tax_retroblast_pq verbose parameter works", {
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)

  # Should work with verbose = FALSE
  expect_no_error(tax_retroblast_pq(data_fungi_mini_cleanNames,
    marker = c("ITS"),
    retmax = 5, id_cut = 99,
    add_to_phyloseq = FALSE,
    verbose = FALSE
  ))
})
