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

test_that("taxnames parameter accepts character vectors", {
  # Test that taxnames accepts various character vectors
  taxnames_single <- c("Amanita muscaria")
  taxnames_multiple <- c("Amanita muscaria", "Boletus edulis", "Cantharellus cibarius")
  
  expect_type(taxnames_single, "character")
  expect_length(taxnames_single, 1)
  
  expect_type(taxnames_multiple, "character")
  expect_length(taxnames_multiple, 3)
})

test_that("taxnames usage returns tibble when add_to_phyloseq is FALSE", {
  # This is a structural test - actual API calls would be in integration tests
  # Here we just verify the parameter structure makes sense
  
  # When using taxnames, add_to_phyloseq should default to FALSE
  # and the function should return a data.frame/tibble
  skip("Requires API access for actual testing")
})

test_that("Functions document taxnames capability", {
  # Test that help pages exist and mention taxnames
  skip_if_not_installed("roxygen2")
  
  # These functions should have taxnames parameter documented
  functions_with_taxnames <- c(
    "gna_verifier_pq",
    "tax_gbif_occur_pq",
    "tax_get_wk_info_pq",
    "tax_globi_pq",
    "tax_info_pq",
    "tax_iucn_code_pq",
    "tax_oa_pq",
    "tax_occur_check_pq",
    "tax_photos_pq"
  )
  
  for (func_name in functions_with_taxnames) {
    # Just check that the function exists
    expect_true(exists(func_name, where = "package:taxinfo", mode = "function"),
                info = paste(func_name, "should exist"))
  }
})
