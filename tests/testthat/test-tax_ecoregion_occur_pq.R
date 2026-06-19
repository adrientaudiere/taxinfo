test_that("tax_ecoregion_occur_pq input validation", {
  expect_error(tax_ecoregion_occur_pq(NULL, NULL))
  expect_error(tax_ecoregion_occur_pq(
    physeq = data_fungi_mini,
    taxnames = "X"
  ))
  expect_error(tax_ecoregion_occur_pq(
    taxnames = "X",
    add_to_phyloseq = TRUE
  ))
})

test_that("tax_ecoregion_occur_pq with taxnames returns tibble + summary attr", {
  skip_on_cran()
  vcr::use_cassette("ecoregion_occur_pq_structure", {
    res <- tax_ecoregion_occur_pq(
      taxnames = "Xylobolus subpileatus",
      n_occur = 20,
      time_to_sleep = 0,
      verbose = FALSE
    )
  })
  expect_s3_class(res, "tbl_df")
  summ <- attr(res, "tax_summary")
  expect_s3_class(summ, "tbl_df")
  expect_true(all(
    c("taxon_name", "ecoregion_top", "ecoregion_n", "ecoregion_list") %in%
      names(summ)
  ))
})

test_that("tax_ecoregion_occur_pq respects col_prefix", {
  skip_on_cran()
  vcr::use_cassette("ecoregion_occur_pq_prefix", {
    res <- tax_ecoregion_occur_pq(
      taxnames = "Xylobolus subpileatus",
      col_prefix = "eco_",
      n_occur = 20,
      time_to_sleep = 0,
      verbose = FALSE
    )
  })
  summ <- attr(res, "tax_summary")
  expect_true(all(c("eco_top", "eco_n", "eco_list") %in% names(summ)))
})
