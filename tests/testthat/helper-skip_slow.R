# A few functions depend on heavy external services that are impractical to
# record with vcr: `range_bioreg_pq()` triggers gbif.range's own GBIF downloads,
# the ~50 MB WWF/TNC ecoregion layer, and rnaturalearth boundaries. Their tests
# are skipped by default and run only when `TAXINFO_TEST_SLOW=true` (set it in
# CI / before a release to exercise these paths).
skip_if_no_slow_tests <- function() {
  if (!identical(Sys.getenv("TAXINFO_TEST_SLOW"), "true")) {
    testthat::skip(
      "Slow / un-cassettable API tests disabled (set TAXINFO_TEST_SLOW=true)"
    )
  }
  testthat::skip_if_offline()
}
