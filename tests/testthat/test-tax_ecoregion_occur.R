test_that("tax_ecoregion_occur input validation", {
  expect_error(tax_ecoregion_occur(NULL))
  expect_error(tax_ecoregion_occur(character()))
  expect_error(tax_ecoregion_occur("Amanita muscaria", min_nb_occur = -1))
  expect_error(tax_ecoregion_occur("Amanita muscaria", min_proportion = -0.1))
  expect_error(tax_ecoregion_occur("Amanita muscaria", min_proportion = 1.2))
})

test_that("tax_ecoregion_occur returns a long tibble with required columns", {
  skip_on_cran()
  vcr::use_cassette("ecoregion_occur_structure", {
    res <- tax_ecoregion_occur(
      "Xylobolus subpileatus",
      n_occur = 20,
      time_to_sleep = 0,
      verbose = FALSE
    )
  })
  expect_s3_class(res, "tbl_df")
  expect_true(all(
    c(
      "taxon_name",
      "ECO_NAME",
      "biome",
      "realm",
      "n_occur",
      "prop_occur"
    ) %in%
      names(res)
  ))
  expect_true(is.integer(res$n_occur))
  expect_true(all(res$taxon_name == "Xylobolus subpileatus"))
})

test_that("tax_ecoregion_occur keeps unmatched taxa with n_occur = 0", {
  skip_on_cran()
  vcr::use_cassette("ecoregion_occur_unmatched", {
    res <- tax_ecoregion_occur(
      c("Xylobolus subpileatus", "NotARealTaxon ZzYy"),
      n_occur = 20,
      time_to_sleep = 0,
      verbose = FALSE
    )
  })
  expect_true("NotARealTaxon ZzYy" %in% res$taxon_name)
  miss <- res[res$taxon_name == "NotARealTaxon ZzYy", ]
  expect_true(all(is.na(miss$ECO_NAME)))
  expect_true(all(miss$n_occur == 0L))
})
