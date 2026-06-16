# Test tax_occur_check function
# Examples from man page: tax_occur_check.Rd

test_that("tax_occur_check input validation", {
  # Test with NULL or invalid taxa names
  expect_error(tax_occur_check(NULL, 2.3522, 48.8566, 100))
  expect_error(suppressWarnings(tax_occur_check("", 2.3522, 48.8566, 100)))

  # Test with invalid coordinates
  expect_error(tax_occur_check("Quercus robur", NULL, 48.8566, 100))
  expect_error(tax_occur_check("Quercus robur", 2.3522, NULL, 100))

  # Test with invalid radius
  expect_error(tax_occur_check("Quercus robur", 2.3522, 48.8566, NULL))
  expect_error(tax_occur_check("Quercus robur", 2.3522, 48.8566, -10))
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

# Examples from man page: tax_occur_check.Rd (lines 74-82)
test_that("tax_occur_check returns correct structure", {
  skip_if_offline()
  skip_on_cran()
  # Example: Q_rob_in_Paris <- tax_occur_check("Quercus robur", long, lat, 100)
  long <- 2.3522
  lat <- 48.8566
  Q_rob_in_Paris <- tax_occur_check("Quercus robur", long, lat, 100)

  # Test return structure is a list
  expect_type(Q_rob_in_Paris, "list")

  # Test expected elements in the list
  expect_true("count_in_radius" %in% names(Q_rob_in_Paris))
  expect_true("closest_distance_km" %in% names(Q_rob_in_Paris))
  expect_true("mean_distance_km" %in% names(Q_rob_in_Paris))
  expect_true("total_count_in_world" %in% names(Q_rob_in_Paris))
  expect_true("search_radius" %in% names(Q_rob_in_Paris))

  # Values should be numeric
  expect_true(is.numeric(Q_rob_in_Paris$count_in_radius))
})

test_that("tax_occur_check with Trametopsis brasiliensis", {
  skip_if_offline()
  skip_on_cran()
  # Example: tax_occur_check("Trametopsis brasiliensis", long, lat, 100)
  long <- 2.3522
  lat <- 48.8566
  result <- tax_occur_check("Trametopsis brasiliensis", long, lat, 100)
  expect_type(result, "list")
  expect_true("count_in_radius" %in% names(result))
})

test_that("tax_occur_check with return_all_occ = TRUE", {
  skip_if_offline()
  skip_on_cran()
  # Example: res_occ <- tax_occur_check("Fagus sylvatica", long, lat, 200, return_all_occ = TRUE)
  long <- 2.3522
  lat <- 48.8566
  res_occ <- tax_occur_check(
    "Fagus sylvatica",
    long,
    lat,
    200,
    return_all_occ = TRUE
  )

  # Should include occ_data element
  expect_type(res_occ, "list")
  expect_true("occ_data" %in% names(res_occ))
  expect_s3_class(res_occ$occ_data, "data.frame")
})
