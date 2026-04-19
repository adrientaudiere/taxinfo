test_that("points_to_ecoregions input validation", {
  expect_error(points_to_ecoregions(NULL, 48))
  expect_error(points_to_ecoregions(2, NULL))
  expect_error(points_to_ecoregions(c(2, 3), 48))
  expect_error(points_to_ecoregions("a", 48))
  expect_error(points_to_ecoregions(200, 48))
  expect_error(points_to_ecoregions(2, 100))
})

test_that("points_to_ecoregions returns a tibble with expected columns", {
  res <- points_to_ecoregions(
    longitudes = c(2.3522, 4.2),
    latitudes = c(48.8566, 33)
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true(all(
    c("point_id", "longitude", "latitude", "ECO_NAME", "biome", "realm") %in%
      names(res)
  ))
  expect_type(res$ECO_NAME, "character")
})

test_that("points_to_ecoregions returns NA for ocean points", {
  res <- points_to_ecoregions(longitudes = 0, latitudes = 0)
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$ECO_NAME))
})
