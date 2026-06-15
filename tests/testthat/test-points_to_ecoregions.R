test_that("points_to_ecoregions input validation", {
  expect_error(points_to_ecoregions(NULL, 48))
  expect_error(points_to_ecoregions(2, NULL))
  expect_error(points_to_ecoregions(c(2, 3), 48))
  expect_error(points_to_ecoregions("a", 48))
  expect_error(points_to_ecoregions(200, 48))
  expect_error(points_to_ecoregions(2, 100))
})

test_that("points_to_ecoregions works offline with a stub ecoregion layer", {
  skip_if_not_installed("sf")
  stub <- sf::st_sf(
    ECO_NAME = "Stubland",
    WWF_MHTNAM = "Stub biome",
    WWF_REALM2 = "Stub realm",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0),
        c(10, 0),
        c(10, 10),
        c(0, 10),
        c(0, 0)
      ))),
      crs = 4326
    )
  )
  res <- points_to_ecoregions(
    longitudes = c(5, 50),
    latitudes = c(5, 50),
    ecoregions = stub
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_equal(res$ECO_NAME[1], "Stubland")
  expect_equal(res$biome[1], "Stub biome")
  expect_equal(res$realm[1], "Stub realm")
  # Point (50, 50) lies outside the stub polygon -> NA ecoregion.
  expect_true(is.na(res$ECO_NAME[2]))
})

test_that("points_to_ecoregions returns a tibble with expected columns", {
  skip_on_cran() # first call may download the ~50 MB WWF/TNC layer
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
  skip_on_cran() # first call may download the ~50 MB WWF/TNC layer
  res <- points_to_ecoregions(longitudes = 0, latitudes = 0)
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$ECO_NAME))
})
