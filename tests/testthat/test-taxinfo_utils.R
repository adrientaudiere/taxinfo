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

# Additional edge cases for calculate_bbox
test_that("calculate_bbox handles boundary coordinates", {
  # Test at international date line (180/-180)
  bbox_dateline_pos <- calculate_bbox(180, 0, 10)
  bbox_dateline_neg <- calculate_bbox(-180, 0, 10)

  expect_type(bbox_dateline_pos, "list")
  expect_type(bbox_dateline_neg, "list")

  # Test at poles (latitude 90/-90)
  # Note: At the pole, longitude adjustment can become very large
  bbox_north_pole <- calculate_bbox(0, 89.5, 10) # Near but not exactly at pole
  bbox_south_pole <- calculate_bbox(0, -89.5, 10)

  expect_type(bbox_north_pole, "list")
  expect_type(bbox_south_pole, "list")
})

test_that("calculate_bbox returns symmetric expansion", {
  # The bbox should be symmetric around the center point
  lon <- 10
  lat <- 45
  radius <- 25

  bbox <- calculate_bbox(lon, lat, radius)

  # xmax - lon should equal lon - xmin
  expect_equal(bbox$xmax - lon, lon - bbox$xmin, tolerance = 1e-10)

  # ymax - lat should equal lat - ymin
  expect_equal(bbox$ymax - lat, lat - bbox$ymin, tolerance = 1e-10)
})

test_that("calculate_bbox latitude offset is consistent", {
  # Latitude offset should be the same regardless of longitude
  bbox1 <- calculate_bbox(0, 45, 50)
  bbox2 <- calculate_bbox(90, 45, 50)
  bbox3 <- calculate_bbox(-120, 45, 50)

  # All should have the same latitude span
  lat_span1 <- bbox1$ymax - bbox1$ymin
  lat_span2 <- bbox2$ymax - bbox2$ymin
  lat_span3 <- bbox3$ymax - bbox3$ymin

  expect_equal(lat_span1, lat_span2, tolerance = 1e-10)
  expect_equal(lat_span2, lat_span3, tolerance = 1e-10)
})

test_that("calculate_bbox with small radius", {
  # Test with very small radius
  bbox_tiny <- calculate_bbox(0, 0, 0.1)

  expect_type(bbox_tiny, "list")
  expect_true(bbox_tiny$xmax > bbox_tiny$xmin)
  expect_true(bbox_tiny$ymax > bbox_tiny$ymin)
})

test_that("calculate_bbox with large radius", {
  # Test with large radius
  bbox_large <- calculate_bbox(0, 0, 500)

  expect_type(bbox_large, "list")
  # At equator, 500 km ≈ 4.5 degrees (500/111.32), so span is ~9 degrees
  # Using 8 degrees as a conservative threshold
  expect_true((bbox_large$xmax - bbox_large$xmin) > 8)
  expect_true((bbox_large$ymax - bbox_large$ymin) > 8)
})

# Note: taxa_summary_text requires phyloseq objects and MiscMetabar package
# These tests would need mock phyloseq objects or be integration tests
test_that("taxa_summary_text input validation", {
  # This is a placeholder for when phyloseq objects are available
  # expect_error(taxa_summary_text(NULL))
  skip("Requires phyloseq objects and MiscMetabar package")
})
