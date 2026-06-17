test_that("has_gbif_credentials reflects environment variables", {
  withr::local_envvar(GBIF_USER = "u", GBIF_PWD = "p", GBIF_EMAIL = "e")
  expect_true(has_gbif_credentials())

  withr::local_envvar(GBIF_USER = "", GBIF_PWD = "", GBIF_EMAIL = "")
  expect_false(has_gbif_credentials())

  withr::local_envvar(GBIF_USER = "u", GBIF_PWD = "", GBIF_EMAIL = "e")
  expect_false(has_gbif_credentials())
})

test_that("check_gbif_credentials aborts when credentials are missing", {
  withr::local_envvar(GBIF_USER = "", GBIF_PWD = "", GBIF_EMAIL = "")
  expect_error(check_gbif_credentials(), "credentials")

  withr::local_envvar(GBIF_USER = "u", GBIF_PWD = "p", GBIF_EMAIL = "e")
  expect_true(check_gbif_credentials())
})

test_that("compute_occur_stats counts occurrences within the radius", {
  occ <- data.frame(
    decimalLongitude = c(0, 1, 0.1),
    decimalLatitude = c(0, 0, 0)
  )
  res <- compute_occur_stats(
    occ,
    longitude = 0,
    latitude = 0,
    radius_km = 50,
    circle_form = TRUE
  )
  # (0, 0) is 0 km and (0.1, 0) is ~11 km away; (1, 0) is ~111 km away.
  expect_equal(res$count_in_radius, 2)
  expect_equal(res$closest_distance_km, 0)
  expect_true(res$mean_distance_km > 0)
})

test_that("compute_occur_stats handles NULL and empty input", {
  expect_equal(
    compute_occur_stats(NULL, 0, 0, 50)$count_in_radius,
    0
  )
  empty <- data.frame(
    decimalLongitude = numeric(),
    decimalLatitude = numeric()
  )
  expect_equal(
    compute_occur_stats(empty, 0, 0, 50)$count_in_radius,
    0
  )
})

test_that("compute_occur_stats circle_form = FALSE keeps all occurrences", {
  occ <- data.frame(
    decimalLongitude = c(0, 1),
    decimalLatitude = c(0, 0)
  )
  res <- compute_occur_stats(occ, 0, 0, 50, circle_form = FALSE)
  expect_equal(res$count_in_radius, 2)
})

test_that("attribute_gbif_records attributes by speciesKey and taxonKey", {
  gbif_taxa <- tibble::tibble(
    usageKey = c(100, 200),
    canonicalName = c("Aaa bbb", "Ccc ddd"),
    verbatim_name = c("Aaa bbb", "Ccc ddd")
  )
  occ <- data.frame(
    taxonKey = c(100, 150, 200),
    speciesKey = c(100, 100, 200),
    species = c("Aaa bbb", "Aaa bbb", "Ccc ddd"),
    genus = c("Aaa", "Aaa", "Ccc"),
    decimalLongitude = c(0, 0, 0),
    decimalLatitude = c(0, 0, 0)
  )
  res <- attribute_gbif_records(occ, gbif_taxa)
  # Record 150 is infraspecific of species 100 (shared speciesKey).
  expect_equal(sum(res$usageKey == 100), 2)
  expect_equal(sum(res$usageKey == 200), 1)
  expect_true(all(c("taxon_name", "usageKey") %in% names(res)))
})

test_that("attribute_gbif_records falls back to name match for higher rank", {
  gbif_taxa <- tibble::tibble(
    usageKey = 999,
    canonicalName = "Quercus",
    verbatim_name = "Quercus"
  )
  occ <- data.frame(
    taxonKey = c(111, 222),
    speciesKey = c(111, 222),
    genus = c("Quercus", "Quercus"),
    species = c("Quercus robur", "Quercus alba"),
    decimalLongitude = c(0, 0),
    decimalLatitude = c(0, 0)
  )
  res <- attribute_gbif_records(occ, gbif_taxa)
  expect_equal(nrow(res), 2)
  expect_true(all(res$usageKey == 999))
})

test_that("build_gbif_coords_sql builds the expected query", {
  sql <- build_gbif_coords_sql(
    keys = c(1, 2),
    country = "FR",
    year_gte = 2000,
    year_lte = 2020
  )
  expect_true(grepl("taxonkey IN \\(1, 2\\)", sql))
  expect_true(grepl("hascoordinate = TRUE", sql))
  expect_true(grepl("countrycode = 'FR'", sql))
  expect_true(grepl("year >= 2000", sql))
  expect_true(grepl("year <= 2020", sql))
})

test_that("bbox_for_points expands around all points", {
  bb <- bbox_for_points(c(0, 2), c(0, 0), radius_km = 111)
  expect_lt(bb$xmin, 0)
  expect_gt(bb$xmax, 2)
  expect_lt(bb$ymin, 0)
  expect_gt(bb$ymax, 0)
})
