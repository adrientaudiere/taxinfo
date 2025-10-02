# Test taxnames parameter usage across functions

test_that("taxnames parameter validation works", {
  # Test that functions accept taxnames parameter
  expect_error(
    tax_gbif_occur_pq(physeq = NULL, taxnames = NULL),
    "You must specify either"
  )

  expect_error(
    tax_iucn_code_pq(physeq = NULL, taxnames = NULL),
    "You must specify either"
  )

  expect_error(
    gna_verifier_pq(physeq = NULL, taxnames = NULL),
    "You must specify either"
  )
})

test_that("taxnames and physeq are mutually exclusive", {
  # These would fail if both are provided
  # Using mock values to test validation
  skip_if_not_installed("phyloseq")

  # Test that providing both physeq and taxnames causes error
  expect_error(
    tax_gbif_occur_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )

  expect_error(
    tax_iucn_code_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("add_to_phyloseq cannot be TRUE with taxnames", {
  # Test that add_to_phyloseq = TRUE with taxnames causes error
  expect_error(
    tax_gbif_occur_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    tax_iucn_code_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    gna_verifier_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    tax_oa_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    tax_info_pq(
      taxnames = c("Amanita muscaria"),
      file_name = tempfile(),
      csv_taxonomic_rank = "genus",
      add_to_phyloseq = TRUE
    ),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    tax_occur_check_pq(
      taxnames = c("Amanita muscaria"),
      longitude = 2.3,
      latitude = 48.8,
      add_to_phyloseq = TRUE
    ),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    tax_photos_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    tax_get_wk_info_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )

  expect_error(
    tax_globi_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )
})
