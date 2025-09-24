# Test plot_tax_gbif_pq function

data_fungi_cleanNames <- gna_verifier_pq(data_fungi,
  add_to_phyloseq = TRUE
)

data_fungi_cleanNames_3sp <- subset_taxa_pq(data_fungi_cleanNames,
  grepl(
    "Sistotrema raduloides|Stypella subgelatinosa|Rhamphoria piriformis",
    data_fungi_cleanNames@tax_table[, "currentCanonicalSimple"]
  ),
  taxa_names_from_physeq = T
)

test_that("plot_tax_gbif_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(plot_tax_gbif_pq(NULL))
})


test_that("plot_tax_gbif_pq plotting functionality", {
  p1 <- plot_tax_gbif_pq(
    data_fungi_cleanNames_3sp,
    hexagons = TRUE,
    verbose = TRUE, bins = 50, occ_samp = 10, grain = 10000
  )
  expect_equal(length(p1), 2)
  expect_s3_class(p1[[1]], "ggplot")

  p2 <- plot_tax_gbif_pq(
    data_fungi_cleanNames_3sp,
    interactive_plot = TRUE,
    countries = "france", bins = 50, occ_samp = 10, grain = 10000
  )
  expect_equal(length(p2), 2)
  expect_s4_class(p2[[1]], "mapview")
})
