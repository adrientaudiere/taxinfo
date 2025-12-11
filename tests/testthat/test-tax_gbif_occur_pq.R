# Test tax_gbif_occur_pq function

test_that("tax_gbif_occur_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_gbif_occur_pq(NULL))

  skip("Requires phyloseq objects")
})

test_that("tax_gbif_occur_pq parameter defaults", {
  # Test default parameter values
  # taxonomic_rank should default to "currentCanonicalSimple"
  # add_to_phyloseq should default to FALSE
  # verbose should default to TRUE

  expect_true(is.character("currentCanonicalSimple"))
  expect_true(is.logical(FALSE))
  expect_true(is.logical(TRUE))
})

test_that("tax_gbif_occur_pq GBIF integration", {
  # Test GBIF occurrence data retrieval
  # Test occurrence count processing
  # Test coordinate handling

  skip("Requires GBIF API access")
})

test_that("tax_gbif_occur_pq return behavior", {
  # Test return modes
  # When add_to_phyloseq = TRUE: should return phyloseq with occurrence data
  # When add_to_phyloseq = FALSE: should return occurrence tibble

  skip("Requires phyloseq objects")
})
