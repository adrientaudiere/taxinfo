add_source_cols <- function(physeq, ids, prefix = "") {
  tax <- as.data.frame(unclass(physeq@tax_table))
  tax[[paste0(prefix, "currentCanonicalSimple_dataSourceId")]] <- ids
  tax[[paste0(prefix, "currentCanonicalSimple_dataSourceTitleShort")]] <- c(
    "1" = "Catalogue of Life",
    "13" = "Catalogue of Life XR",
    "11" = "GBIF Backbone Taxonomy",
    "210" = "TAXREF"
  )[ids]
  physeq@tax_table <- phyloseq::tax_table(as.matrix(tax))
  physeq
}

test_that("check_backbone_pq informs on a backbone mismatch", {
  pq <- add_source_cols(
    load_clean_pq(),
    c("210", "210", "11", "11", "11", "11", NA)
  )
  expect_message(
    res <- check_backbone_pq(pq, checklist = "backbone"),
    "another backbone"
  )
  expect_equal(res$n_taxa[res$dataSourceId %in% "210"], 2)
  expect_false(res$expected[res$dataSourceId %in% "210"])
  expect_true(res$expected[res$dataSourceId %in% "11"])
  expect_true(is.na(res$expected[is.na(res$dataSourceId)]))
})

test_that("check_backbone_pq is silent when sources match", {
  pq <- add_source_cols(load_clean_pq(), rep("11", 7))
  expect_no_message(check_backbone_pq(pq, checklist = "backbone"))
  expect_no_message(check_backbone_pq(pq, expected_source = c(1, 11)))
  expect_message(check_backbone_pq(pq, checklist = "colxr"), "another backbone")
})

test_that("check_backbone_pq handles missing or prefixed source columns", {
  pq <- load_clean_pq()
  expect_message(res <- check_backbone_pq(pq), "No GNA data source")
  expect_null(res)
  expect_no_message(check_gbif_backbone(pq, checklist = "backbone"))

  pq_pref <- add_source_cols(pq, rep("210", 7), prefix = "gna_")
  expect_message(
    check_backbone_pq(
      pq_pref,
      checklist = "backbone",
      taxonomic_rank = "gna_currentCanonicalSimple"
    ),
    "another backbone"
  )
  expect_null(check_backbone_pq(
    pq_pref,
    checklist = "backbone",
    taxonomic_rank = c("Genus", "Species"),
    verbose = FALSE
  ))
  expect_error(check_backbone_pq(pq, source_col = "nope"), "not a column")
  expect_error(check_backbone_pq("not a phyloseq"), "phyloseq object")
})

test_that("check_backbone_pq skips custom GBIF checklists", {
  pq <- add_source_cols(load_clean_pq(), rep("210", 7))
  expect_message(
    res <- check_backbone_pq(
      pq,
      checklist = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
    ),
    "cannot be mapped"
  )
  expect_null(res)
})

test_that("GBIF functions report a backbone mismatch", {
  pq <- add_source_cols(load_clean_pq(), rep("210", 7))
  testthat::local_mocked_bindings(
    gbif_match_taxa = function(...) {
      tibble::tibble(
        usageKey = character(),
        canonicalName = character(),
        verbatim_name = character()
      )
    }
  )
  withr::local_options(taxinfo.gbif_checklist = "backbone")
  msgs <- capture_messages(tax_gbif_alt(pq, verbose = TRUE))
  expect_true(any(grepl("another backbone", msgs)))
  expect_no_message(tax_gbif_alt(pq, verbose = FALSE))
})

test_that("check_backbone_pq maps COL XR to GNA data sources 13 and 1", {
  pq <- add_source_cols(load_clean_pq(), c(rep("13", 4), rep("1", 3)))
  expect_no_message(check_backbone_pq(pq, checklist = "colxr"))
  expect_message(
    check_backbone_pq(pq, checklist = "backbone"),
    "data_sources = 11"
  )
  pq_11 <- add_source_cols(load_clean_pq(), rep("11", 7))
  expect_message(
    check_backbone_pq(pq_11, checklist = "colxr"),
    "data_sources = 13"
  )
})

test_that("check_backbone_pq keeps one data source per verified column", {
  pq <- load_clean_pq() |>
    add_source_cols(rep("210", 7), prefix = "taxref_") |>
    add_source_cols(rep("11", 7), prefix = "gbif_")
  expect_no_message(check_backbone_pq(
    pq,
    checklist = "backbone",
    taxonomic_rank = "gbif_currentCanonicalSimple"
  ))
  expect_message(
    res <- check_backbone_pq(
      pq,
      checklist = "backbone",
      taxonomic_rank = "taxref_currentCanonicalSimple"
    ),
    "TAXREF"
  )
  expect_equal(res$dataSourceId, "210")
  expect_message(
    check_backbone_pq(
      pq,
      checklist = "backbone",
      taxonomic_rank = "taxref_genusEpithet"
    ),
    "TAXREF"
  )
  expect_null(check_backbone_pq(pq, checklist = "backbone", verbose = FALSE))
})

test_that("check_source_backbone informs for a non-GBIF database", {
  pq <- add_source_cols(load_clean_pq(), rep("11", 7))
  expect_message(
    check_source_backbone(pq, expected_source = 4L),
    "data_sources = 4"
  )
  expect_no_message(check_source_backbone(pq, expected_source = 11L))
  expect_no_message(check_source_backbone(NULL, expected_source = 4L))
})

test_that("NCBI and Wikidata functions check their GNA data source", {
  testthat::local_mocked_bindings(
    check_source_backbone = function(physeq, expected_source, ...) {
      cli::cli_abort("expected source {expected_source}")
    }
  )
  pq <- load_clean_pq()
  skip_if_not_installed("rentrez")
  expect_error(tax_retroblast_pq(pq), "expected source 4")
  skip_if_not_installed("wikitaxa")
  expect_error(tax_get_wk_info_pq(pq), "expected source 207")
  expect_error(
    tax_photos_pq(pq, source = "wikitaxa"),
    "expected source 207"
  )
})
