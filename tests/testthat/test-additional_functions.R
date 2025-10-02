# Test additional functions with basic validation
data_fungi_cleanNames <- gna_verifier_pq(data_fungi,
  add_to_phyloseq = TRUE
)

data_fungi_cleanNames_3sp <-
  suppressWarnings(select_taxa_pq(data_fungi_cleanNames,
    taxnames = c("Sistotrema raduloides", "Stypella subgelatinosa", "Rhamphoria piriformis"),
    clean_pq = TRUE
  ))

test_that("tax_get_wk_pages_info input validation", {
  expect_error(tax_get_wk_pages_info("Q10723171", tib_list = pages_Q10723171))

  ti1 <- tax_get_wk_pages_info("Q10723171")
  expect_length(ti1, 2)


  ti2 <- tax_get_wk_pages_info("Q10723171",
    languages_pages = c("fr", "en"),
    summarize_function_length = "sum"
  )
  expect_length(ti2, 2)
  expect_gt(ti1$page_views, ti2$page_views)

  pages_Q10723171 <- tax_get_wk_lang("Q10723171")
  ti3 <- tax_get_wk_pages_info(tib_list = pages_Q10723171, n_days = 3)
  expect_length(ti3, 2)
})

# Test tax_get_wk_info_pq function
test_that("tax_get_wk_info_pq input validation", {
  expect_error(tax_get_wk_info_pq(NULL))

  res1 <- tax_get_wk_info_pq(data_fungi_cleanNames_3sp, languages_pages = c("en"), time_to_sleep = 2)
  expect_equal(nrow(res1), 2)

  res2 <- tax_get_wk_info_pq(data_fungi_cleanNames_3sp, add_to_phyloseq = TRUE, time_to_sleep = 2)
  expect_s4_class(res2, "phyloseq")
})

# Test tax_globi_pq function
test_that("tax_globi_pq input validation", {
  expect_error(tax_globi_pq(NULL))
  res1 <- tax_globi_pq(data_fungi_cleanNames_3sp,
    taxonomic_rank = c("Genus", "Species"),
    interaction_types = list("parasiteOf", "hasHost"),
    verbose = TRUE,
    max_interactions = 10
  )
  expect_equal(dim(res1), c(2, 4))

  res2 <- tax_globi_pq(data_fungi_cleanNames_3sp,
    taxonomic_rank = c("Genus", "Species"),
    interaction_types = list("parasiteOf", "hasHost"),
    verbose = TRUE,
    max_interactions = 10
  )
  expect_s4_class(res2, "phyloseq")
})

# Test tax_iucn_code_pq function
test_that("tax_iucn_code_pq input validation", {
  expect_error(tax_iucn_code_pq(NULL))
  res1 <- tax_iucn_code_pq(data_fungi_cleanNames)
  expect_equal(dim(res1), c(254, 2))

  res2 <- tax_iucn_code_pq(data_fungi_cleanNames, )

  expect_s4_class(res2, "phyloseq")
})

# Test tax_retroblast_pq function
test_that("tax_retroblast_pq input validation", {
  expect_error(tax_retroblast_pq(NULL))
  res1 <- tax_retroblast_pq(data_fungi_cleanNames_3sp,
    marker = c("ITS", "internal transcribed spacer"),
    retmax = 10, id_cut = 99
  )
  expect_equal(length(res1), 2)
  expect_equal(nrow(res1$tib_retroblast), 6)

  res2 <- tax_retroblast_pq(data_fungi_cleanNames_3sp,
    marker = c("ITS", "internal transcribed spacer"),
    retmax = 10, id_cut = 99
  )
  expect_s4_class(res2, "phyloseq")
})

# Test tax_check_ecoregion function
test_that("tax_check_ecoregion input validation", {
  # Test coordinate validation
  expect_error(tax_check_ecoregion(NULL, 0))
  expect_error(tax_check_ecoregion(0, NULL))

  # Test coordinate ranges
  expect_error(tax_check_ecoregion(-200, 0)) # Invalid longitude
  expect_error(tax_check_ecoregion(200, 0)) # Invalid longitude
  expect_error(tax_check_ecoregion(0, -100)) # Invalid latitude
  expect_error(tax_check_ecoregion(0, 100)) # Invalid latitude

  res1 <- suppressWarnings(tax_check_ecoregion("Xylobolus subpileatus",
    longitudes = c(2.3522, 4.2),
    latitudes = c(48.8566, 33)
  ))
  expect_equal(length(res1), 3)
  expect_false(res1$is_in_ecoregion)
})

# Test range_bioreg_pq function
test_that("range_bioreg_pq input validation", {
  expect_error(range_bioreg_pq(NULL))
  res1 <- range_bioreg_pq(data_fungi_cleanNames_3sp, occ_samp = 100)
  expect_equal(length(res1), 2)

  p <- range_bioreg_pq(data_fungi_cleanNames_3sp,
    occ_samp = 100,
    make_plot = TRUE
  )
  expect_equal(length(p), 2)
  expect_s3_class(p[[1]], "ggplot")
  unlink("inst", recursive = TRUE)
})
