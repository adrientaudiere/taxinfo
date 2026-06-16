# Test taxa_summary_text function
# Examples from man page: taxa_summary_text.Rd

test_that("taxa_summary_text input validation", {
  # Test with NULL phyloseq object
  expect_error(taxa_summary_text(NULL))
})

test_that("taxa_summary_text aborts clearly when no taxa match", {
  expect_error(
    taxa_summary_text(
      data_fungi,
      taxonomic_rank = c("Genus", "Species"),
      taxnames = "Definitely Notataxon",
      verbose = FALSE
    ),
    "No taxa match"
  )
})

# Examples from man page: taxa_summary_text.Rd (lines 41-50)
test_that("taxa_summary_text returns character string", {
  skip_if_offline()
  skip_on_cran()
  # Example: taxa_summary_text(data_fungi_cleanNames, taxnames = "Xylodon flaviporus")
  data_fungi_cleanNames <- gna_verifier_pq(data_fungi)
  result <- taxa_summary_text(
    data_fungi_cleanNames,
    taxnames = "Xylodon flaviporus"
  )

  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("taxa_summary_text with min_nb_seq parameter", {
  skip_if_offline()
  skip_on_cran()
  # Example: taxa_summary_text(data_fungi_cleanNames, taxnames = "Xylodon flaviporus",
  #   min_nb_seq = 100, verbose = FALSE)
  data_fungi_cleanNames <- gna_verifier_pq(data_fungi)
  result <- taxa_summary_text(
    data_fungi_cleanNames,
    taxnames = "Xylodon flaviporus",
    min_nb_seq = 100,
    verbose = FALSE
  )

  expect_type(result, "character")
})

test_that("taxa_summary_text with different taxonomic_rank", {
  skip_if_offline()
  skip_on_cran()
  # Example: taxa_summary_text(data_fungi_cleanNames, taxonomic_rank = "Trait",
  #   taxnames = c("Soft Rot"), verbose = FALSE)
  data_fungi_cleanNames <- gna_verifier_pq(data_fungi)
  result <- taxa_summary_text(
    data_fungi_cleanNames,
    taxonomic_rank = "Trait",
    taxnames = c("Soft Rot"),
    verbose = FALSE
  )

  expect_type(result, "character")
})
