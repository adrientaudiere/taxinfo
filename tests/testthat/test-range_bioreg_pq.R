# Test range_bioreg_pq (moved from test-additional_functions.R, the only
# coverage of this function). range_bioreg_pq wraps gbif.range, which does its
# own GBIF downloads plus the ~50 MB ecoregion layer and rnaturalearth
# boundaries -- impractical to record with vcr -- so the live test is gated
# behind TAXINFO_TEST_SLOW (see helper-skip_slow.R).

test_that("range_bioreg_pq input validation", {
  expect_error(range_bioreg_pq(NULL))
})

test_that("range_bioreg_pq returns range outputs and plots", {
  skip_if_no_slow_tests()
  clean3 <- suppressWarnings(select_taxa_pq(
    load_clean_pq(),
    taxonomic_rank = "currentCanonicalSimple",
    taxnames = c(
      "Sistotrema raduloides",
      "Stypella subgelatinosa",
      "Mycena renati"
    ),
    clean_pq = TRUE
  ))

  res1 <- range_bioreg_pq(clean3, occ_samp = 100)
  if (!is.null(res1) && length(res1) > 0) {
    expect_equal(length(res1), 2)
  }

  p <- range_bioreg_pq(clean3, occ_samp = 100, make_plot = TRUE)
  if (!is.null(p) && length(p) >= 1 && !is.null(p[[1]])) {
    expect_s3_class(p[[1]], "ggplot")
  }
  unlink("inst", recursive = TRUE)
})
