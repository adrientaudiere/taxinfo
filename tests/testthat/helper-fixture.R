# Shared, pre-cleaned phyloseq fixture for the test suite.
#
# `load_clean_pq()` returns a small phyloseq already processed by
# `gna_verifier_pq()` (it carries the `currentCanonicalSimple` column the
# `tax_*_pq()` functions need). Use it instead of calling `gna_verifier_pq()`
# in each test: it is fast, offline and deterministic. See
# fixtures/make-data_fungi_clean.R for how the object is built.
#
# Canonical names available in the fixture (assert against these):
#   Basidiodendron eyrei, Fomes fomentarius, Mycena renati,
#   Sistotrema raduloides, Stypella subgelatinosa, Xylodon flaviporus

load_clean_pq <- function() {
  readRDS(testthat::test_path("fixtures", "data_fungi_clean.rds"))
}
