# Test utilities functions

test_that("calculate_bbox works correctly", {
  # Test basic functionality
  bbox <- calculate_bbox(2.3522, 48.8566, 50)

  expect_type(bbox, "list")
  expect_named(bbox, c("xmin", "xmax", "ymin", "ymax"))

  # Check that all values are numeric
  expect_true(all(sapply(bbox, is.numeric)))

  # Check that coordinates are expanded properly
  expect_true(bbox$xmax > 2.3522)
  expect_true(bbox$xmin < 2.3522)
  expect_true(bbox$ymax > 48.8566)
  expect_true(bbox$ymin < 48.8566)

  # Test with different radius
  bbox_small <- calculate_bbox(2.3522, 48.8566, 10)
  bbox_large <- calculate_bbox(2.3522, 48.8566, 100)

  # Larger radius should create larger bbox
  expect_true((bbox_large$xmax - bbox_large$xmin) > (bbox_small$xmax - bbox_small$xmin))
  expect_true((bbox_large$ymax - bbox_large$ymin) > (bbox_small$ymax - bbox_small$ymin))

  # Test edge cases
  expect_error(calculate_bbox(NULL, 48.8566, 50))
  expect_error(calculate_bbox(2.3522, NULL, 50))
  expect_error(calculate_bbox(2.3522, 48.8566, NULL))
})

test_that("calculate_bbox handles different coordinates", {
  # Test extreme coordinates
  bbox_north <- calculate_bbox(0, 80, 50) # Near north pole
  bbox_south <- calculate_bbox(0, -80, 50) # Near south pole
  bbox_equator <- calculate_bbox(0, 0, 50) # At equator

  expect_type(bbox_north, "list")
  expect_type(bbox_south, "list")
  expect_type(bbox_equator, "list")

  # Longitude adjustment should be more extreme at poles
  lon_diff_north <- bbox_north$xmax - bbox_north$xmin
  lon_diff_equator <- bbox_equator$xmax - bbox_equator$xmin
  expect_true(lon_diff_north > lon_diff_equator)
})

# Note: taxa_summary_text requires phyloseq objects and MiscMetabar package
# These tests would need mock phyloseq objects or be integration tests
test_that("taxa_summary_text input validation", {
  # This is a placeholder for when phyloseq objects are available
  # expect_error(taxa_summary_text(NULL))
  skip("Requires phyloseq objects and MiscMetabar package")
})
