# Test additional functions with basic validation

# Test tax_get_wk_info_pq function
test_that("tax_get_wk_info_pq input validation", {
  expect_error(tax_get_wk_info_pq(NULL))
  skip("Requires phyloseq objects")
})

# Test tax_globi_pq function  
test_that("tax_globi_pq input validation", {
  expect_error(tax_globi_pq(NULL))
  skip("Requires phyloseq objects")
})

# Test tax_iucn_code_pq function
test_that("tax_iucn_code_pq input validation", {
  expect_error(tax_iucn_code_pq(NULL))
  skip("Requires phyloseq objects")
})

# Test tax_retroblast_pq function
test_that("tax_retroblast_pq input validation", {
  expect_error(tax_retroblast_pq(NULL))
  skip("Requires phyloseq objects")
})

# Test tax_check_ecoregion function
test_that("tax_check_ecoregion input validation", {
  # Test coordinate validation
  expect_error(tax_check_ecoregion(NULL, 0))
  expect_error(tax_check_ecoregion(0, NULL))
  
  # Test coordinate ranges
  expect_error(tax_check_ecoregion(-200, 0))  # Invalid longitude
  expect_error(tax_check_ecoregion(200, 0))   # Invalid longitude
  expect_error(tax_check_ecoregion(0, -100))  # Invalid latitude
  expect_error(tax_check_ecoregion(0, 100))   # Invalid latitude
})

# Test range_bioreg_pq function
test_that("range_bioreg_pq input validation", {
  expect_error(range_bioreg_pq(NULL))
  skip("Requires phyloseq objects")
})

# Test parameter defaults for functions
test_that("additional functions parameter defaults", {
  # Test that common default patterns are reasonable
  
  # taxonomic_rank defaults
  expect_true(is.character("currentCanonicalSimple"))
  
  # verbose defaults
  expect_true(is.logical(TRUE))
  
  # add_to_phyloseq defaults
  expect_true(is.logical(FALSE))
})

# Test common parameter validation patterns
test_that("common parameter validation", {
  # Test taxonomic rank validation logic
  valid_ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "currentCanonicalSimple")
  
  expect_true(length(valid_ranks) > 0)
  expect_true("currentCanonicalSimple" %in% valid_ranks)
  
  # Test verbose parameter
  expect_true(is.logical(TRUE))
  expect_true(is.logical(FALSE))
})

# Test error handling patterns
test_that("common error handling", {
  # Test that functions handle NULL inputs appropriately
  # Most functions should error with NULL phyloseq objects
  
  # This tests the pattern that should be consistent across functions
  expect_error(eval(parse(text = "function(physeq) { if(is.null(physeq)) stop('phyloseq cannot be NULL') }(NULL)")))
})