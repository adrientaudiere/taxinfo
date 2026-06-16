skip_if_offline()
skip_on_cran()

# Test plot_tax_gbif_pq function
# Examples from man page: plot_tax_gbif_pq.Rd

data_fungi_cleanNames <- gna_verifier_pq(data_fungi)

data_fungi_cleanNames_3sp <- subset_taxa_pq(
  data_fungi_cleanNames,
  grepl(
    "Sistotrema raduloides|Stypella subgelatinosa|Rhamphoria piriformis",
    data_fungi_cleanNames@tax_table[, "currentCanonicalSimple"]
  ),
  taxa_names_from_physeq = TRUE
)

test_that("plot_tax_gbif_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(plot_tax_gbif_pq(NULL))
})


test_that("plot_tax_gbif_pq plotting functionality", {
  p1 <- plot_tax_gbif_pq(
    data_fungi_cleanNames_3sp,
    hexagons = TRUE,
    verbose = TRUE,
    bins = 50,
    occ_samp = 10,
    grain = 10000
  )
  expect_equal(length(p1), 2)
  expect_s3_class(p1[[1]], "ggplot")

  p2 <- plot_tax_gbif_pq(
    data_fungi_cleanNames_3sp,
    interactive_plot = TRUE,
    countries = "france",
    bins = 50,
    occ_samp = 10,
    grain = 10000
  )
  expect_equal(length(p2), 2)
  expect_s4_class(p2[[1]], "mapview")
})

# Examples from man page: plot_tax_gbif_pq.Rd (lines 62-106)
test_that("plot_tax_gbif_pq with taxnames parameter returns list of ggplots", {
  # Example: p <- plot_tax_gbif_pq(taxnames = c("Xylobolus subpileatus", "Stereum subpileatus"))
  p <- plot_tax_gbif_pq(
    taxnames = c("Xylobolus subpileatus", "Stereum subpileatus")
  )

  expect_type(p, "list")
  expect_true(length(p) >= 1)
})

test_that("plot_tax_gbif_pq with hexagons = TRUE and taxnames", {
  # Example: p <- plot_tax_gbif_pq(taxnames = c("Xylobolus subpileatus", "Stereum  subpileatus"),
  #   hexagons = TRUE, verbose = FALSE)
  p <- plot_tax_gbif_pq(
    taxnames = c("Xylobolus subpileatus", "Stereum subpileatus"),
    hexagons = TRUE,
    verbose = FALSE
  )

  expect_type(p, "list")
})

test_that("plot_tax_gbif_pq with countries filter", {
  # Example: p <- plot_tax_gbif_pq(taxnames = c("Xylobolus subpileatus", "Stereum subpileatus"),
  #   hexagons = TRUE, verbose = FALSE, countries = c("france", "spain"))
  p <- plot_tax_gbif_pq(
    taxnames = c("Xylobolus subpileatus", "Stereum subpileatus"),
    hexagons = TRUE,
    verbose = FALSE,
    countries = c("france", "spain")
  )

  expect_type(p, "list")
})

test_that("plot_tax_gbif_pq both physeq and taxnames errors", {
  expect_error(
    plot_tax_gbif_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("plot_tax_gbif_pq neither physeq nor taxnames errors", {
  expect_error(
    plot_tax_gbif_pq(physeq = NULL, taxnames = NULL),
    "You must specify either"
  )
})

test_that("plot_tax_gbif_pq verbose parameter works", {
  # Should work with verbose = FALSE
  expect_no_error(
    plot_tax_gbif_pq(
      taxnames = c("Amanita muscaria"),
      verbose = FALSE,
      occ_samp = 10
    )
  )
})
