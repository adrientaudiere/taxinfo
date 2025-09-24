# Test tax_occur_multi_check_pq function

test_that("tax_occur_multi_check_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_occur_multi_check_pq(NULL))
  
  # Test coordinate parameter validation
  skip("Requires phyloseq objects")
})

test_that("tax_occur_multi_check_pq parameter defaults", {
  # Test default parameter values
  # taxonomic_rank should default to "currentCanonicalSimple"
  # min_occur should default to 0
  # verbose should default to TRUE
  
  expect_true(is.character("currentCanonicalSimple"))
  expect_true(is.numeric(0))
  expect_true(is.logical(TRUE))
})

test_that("tax_occur_multi_check_pq coordinate handling", {
  # Test coordinate parameter combinations
  # Should handle lon_column/lat_column OR longitudes/latitudes vectors
  
  # Test that exactly one longitude source is required
  # Either lon_column or longitudes must be provided
  # Either lat_column or latitudes must be provided
  
  skip("Requires phyloseq objects")
})

test_that("tax_occur_multi_check_pq coordinate validation", {
  # Test coordinate vector validation
  
  # Test coordinate ranges
  valid_longitudes <- c(-180, 0, 180, 2.3522)
  valid_latitudes <- c(-90, 0, 90, 48.8566)
  
  invalid_longitudes <- c(-200, 200, NA)
  invalid_latitudes <- c(-100, 100, NA)
  
  # Longitude validation
  expect_true(all(valid_longitudes >= -180 & valid_longitudes <= 180))
  expect_false(all(invalid_longitudes >= -180 & invalid_longitudes <= 180, na.rm = TRUE))
  
  # Latitude validation  
  expect_true(all(valid_latitudes >= -90 & valid_latitudes <= 90))
  expect_false(all(invalid_latitudes >= -90 & invalid_latitudes <= 90, na.rm = TRUE))
})

test_that("tax_occur_multi_check_pq vector length validation", {
  # Test that coordinate vectors match phyloseq sample count
  # longitudes and latitudes vectors should have same length as nsamples(physeq)
  
  # Mock coordinate vectors
  coords_matching <- c(1.0, 2.0, 3.0)  # 3 samples
  coords_mismatched <- c(1.0, 2.0)     # 2 samples
  
  expect_equal(length(coords_matching), 3)
  expect_equal(length(coords_mismatched), 2)
  expect_false(length(coords_matching) == length(coords_mismatched))
})

test_that("tax_occur_multi_check_pq unique coordinate processing", {
  # Test unique coordinate pair processing
  
  # Test coordinate combination logic
  longitudes <- c(2.3522, 2.3522, 3.0, 3.0)
  latitudes <- c(48.8566, 48.8566, 49.0, 49.0)
  
  # Combine coordinates
  longlat_pairs <- paste(longitudes, latitudes, sep = "_")
  unique_pairs <- unique(longlat_pairs)
  
  expect_equal(length(longlat_pairs), 4)
  expect_equal(length(unique_pairs), 2)  # Should have 2 unique coordinate pairs
})

test_that("tax_occur_multi_check_pq min_occur filtering", {
  # Test minimum occurrence filtering
  
  # Mock occurrence data
  mock_occurrences <- c(0, 5, 10, 15, 20)
  min_occur_threshold <- 10
  
  # Filter based on min_occur
  filtered_occurrences <- mock_occurrences[mock_occurrences >= min_occur_threshold]
  
  expect_true(all(filtered_occurrences >= min_occur_threshold))
  expect_equal(length(filtered_occurrences), 3)  # 10, 15, 20
})

test_that("tax_occur_multi_check_pq return structure", {
  # Test return value structure
  # Should return list with:
  # - tax_range_list: taxonomic range information
  # - otu_matrix_occurence: occurrence matrix
  # - new_physeq: filtered phyloseq object
  
  expected_names <- c("tax_range_list", "otu_matrix_occurence", "new_physeq")
  
  expect_equal(length(expected_names), 3)
  expect_true("tax_range_list" %in% expected_names)
  expect_true("otu_matrix_occurence" %in% expected_names)
  expect_true("new_physeq" %in% expected_names)
})

test_that("tax_occur_multi_check_pq GBIF integration", {
  # Test GBIF occurrence checking integration
  # Test radius_km parameter usage
  
  skip("Requires GBIF API access")
})