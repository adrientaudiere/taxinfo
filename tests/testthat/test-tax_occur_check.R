# Test tax_occur_check function

test_that("tax_occur_check input validation", {
  # Test with NULL or invalid taxa names
  expect_error(tax_occur_check(NULL, 2.3522, 48.8566, 100))
  expect_error(tax_occur_check("", 2.3522, 48.8566, 100))

  # Test with invalid coordinates
  expect_error(tax_occur_check("Quercus robur", NULL, 48.8566, 100))
  expect_error(tax_occur_check("Quercus robur", 2.3522, NULL, 100))

  # Test with invalid radius
  expect_error(tax_occur_check("Quercus robur", 2.3522, 48.8566, NULL))
  expect_error(tax_occur_check("Quercus robur", 2.3522, 48.8566, -10))
})

test_that("tax_occur_check parameter defaults", {
  # Test default parameter values
  # radius_km should default to 50
  # circle_form should default to TRUE
  # clean_coord should default to TRUE
  # return_all_occ should default to FALSE
  # verbose should default to TRUE
  # clean_coord_verbose should default to FALSE
  # n_occur should default to 1000

  # These would be tested with actual API calls or mocked responses
  skip("Requires GBIF API access or mocking")
})

test_that("tax_occur_check coordinates validation", {
  # Test coordinate validation logic
  # Longitude should be between -180 and 180
  # Latitude should be between -90 and 90

  # Invalid longitude
  expect_error(tax_occur_check("Quercus robur", 200, 48.8566, 100))
  expect_error(tax_occur_check("Quercus robur", -200, 48.8566, 100))

  # Invalid latitude
  expect_error(tax_occur_check("Quercus robur", 2.3522, 100, 100))
  expect_error(tax_occur_check("Quercus robur", 2.3522, -100, 100))
})

test_that("tax_occur_check bbox calculation integration", {
  # Test that bounding box calculation is used correctly
  # This would verify the integration with calculate_bbox function
  skip("Requires GBIF API or mocking")
})

test_that("tax_occur_check return structure", {
  # Test the structure of returned data
  # Should return a list with specific named elements when return_all_occ = TRUE
  # Should return simpler structure when return_all_occ = FALSE
  skip("Requires GBIF API or mocking")
})

test_that("tax_occur_check info_names parameter", {
  # Test that info_names parameter controls which columns are returned
  # Default should include: decimalLongitude, decimalLatitude, country, year,
  # scientificName, recordedBy, gbifRegion
  skip("Requires GBIF API or mocking")
})

test_that("tax_occur_check verbose output", {
  # Test that verbose parameter controls message output
  skip("Requires GBIF API or mocking")
})
