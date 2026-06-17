test_that("tax_check_ecoregion input validation", {
  expect_error(tax_check_ecoregion(
    taxnames = "X",
    longitudes = c(1, 2),
    latitudes = 1
  ))
  expect_error(tax_check_ecoregion(
    longitudes = 1,
    latitudes = 1
  ))
  expect_error(tax_check_ecoregion(
    taxnames = "X",
    physeq = data_fungi_mini,
    longitudes = 1,
    latitudes = 1
  ))
  expect_error(tax_check_ecoregion(
    taxnames = "X",
    longitudes = 200,
    latitudes = 1
  ))
  expect_error(tax_check_ecoregion(
    taxnames = "X",
    longitudes = 1,
    latitudes = 100
  ))
})

test_that("tax_check_ecoregion returns expected structure", {
  skip_on_cran()
  res <- tax_check_ecoregion(
    taxnames = "Xylobolus subpileatus",
    longitudes = c(2.3522, 4.2),
    latitudes = c(48.8566, 33),
    n_occur = 100,
    time_to_sleep = 0,
    verbose = FALSE
  )
  expect_type(res, "list")
  expect_named(
    res,
    c("taxon_ecoregions", "points_ecoregion", "is_in_ecoregion", "ecoregion")
  )
  expect_true(is.matrix(res$is_in_ecoregion))
  expect_equal(dim(res$is_in_ecoregion), c(1, 2))
  expect_equal(rownames(res$is_in_ecoregion), "Xylobolus subpileatus")
  expect_equal(colnames(res$is_in_ecoregion), c("point_1", "point_2"))
  expect_true(is.logical(res$is_in_ecoregion))
  expect_s3_class(res$taxon_ecoregions, "tbl_df")
  expect_s3_class(res$points_ecoregion, "tbl_df")
})
