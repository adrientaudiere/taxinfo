# Simulates GBIF name matching where none of the queried names matches (the
# real response then has no usageKey / canonicalName column), and makes any
# GBIF occurrence, usage or download request fail the test.
local_gbif_no_match <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    gbif_backbone_checklist = function(name_data, checklist = NULL, ...) {
      tibble::tibble(
        matchType = "NONE",
        verbatim_name = as.character(name_data)
      )
    },
    gbif_download = function(...) stop("unexpected GBIF download"),
    .env = env
  )
  testthat::local_mocked_bindings(
    occ_search = function(...) stop("unexpected GBIF call"),
    occ_count = function(...) stop("unexpected GBIF call"),
    name_usage = function(...) stop("unexpected GBIF call"),
    .package = "rgbif",
    .env = env
  )
}

no_match_physeq <- function() {
  data("data_fungi_mini", package = "MiscMetabar", envir = environment())
  phyloseq::prune_samples(
    phyloseq::sample_names(data_fungi_mini)[1:3],
    data_fungi_mini
  )
}
