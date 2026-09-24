test_that("gbif_checklist_key maps names, options and raw UUIDs", {
  expect_equal(
    gbif_checklist_key("colxr"),
    "7ddf754f-d193-4cc9-b351-99906754a03b"
  )
  expect_equal(
    gbif_checklist_key("backbone"),
    "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
  )
  expect_equal(
    gbif_checklist_key("7ddf754f-d193-4cc9-b351-99906754a03b"),
    "7ddf754f-d193-4cc9-b351-99906754a03b"
  )
  expect_equal(
    gbif_checklist_key(),
    gbif_checklist_key(gbif_ambient_checklist())
  )

  op <- options(taxinfo.gbif_checklist = "backbone")
  on.exit(options(op), add = TRUE)
  expect_equal(gbif_checklist_key(), gbif_checklist_key("backbone"))
  expect_equal(gbif_checklist_key(NULL), gbif_checklist_key("backbone"))
})

test_that("gbif_checklist_key rejects unknown checklist names", {
  expect_error(gbif_checklist_key("not_a_checklist"))
})

test_that("gbif_key_to_col wraps rgbif::gbif_to_col with a version guard", {
  if ("gbif_to_col" %in% getNamespaceExports("rgbif")) {
    skip(
      "rgbif >= 3.9.0 installed: conversion delegated to rgbif::gbif_to_col()"
    )
  }
  expect_error(gbif_key_to_col(5231190), "gbif_to_col")
})

test_that("gbif_key_to_col validates its input before any call", {
  expect_error(gbif_key_to_col(c("Q2M4", "9WLSS")), "Backbone numeric keys")
  expect_error(gbif_key_to_col(data.frame(key = 1)), "usageKey")
})

test_that("gbif_key_to_col returns one COL XR key per input key", {
  skip_if_not(
    "gbif_to_col" %in% getNamespaceExports("rgbif"),
    "rgbif < 3.9.0: no gbif_to_col()"
  )
  sent <- NULL
  local_mocked_bindings(
    gbif_to_col = function(key, checklistKey = NULL, ...) {
      sent <<- key
      out <- lapply(key, function(k) {
        if (k == "999") {
          list(gbif_key = k)
        } else {
          list(gbif_key = k, usage = list(key = paste0("C", k)))
        }
      })
      if (length(key) == 1) out[[1]] else out
    },
    .package = "rgbif"
  )

  expect_equal(
    gbif_key_to_col(c(5231190, NA, 999, 5231190, 2e6)),
    c("C5231190", NA, NA, "C5231190", "C2000000")
  )
  expect_equal(sent, c("5231190", "999", "2000000"))

  expect_equal(gbif_key_to_col("5451774"), "C5451774")
  expect_equal(gbif_key_to_col(c(NA, NA)), c(NA_character_, NA_character_))

  df <- data.frame(usageKey = c("5231190", NA), name = c("a", "b"))
  res <- gbif_key_to_col(df)
  expect_equal(res$col_key, c("C5231190", NA))
  expect_equal(res$name, df$name)
})

test_that("gbif_name_usage rejects COL XR keys before any network call", {
  expect_error(gbif_name_usage("Q2M4"), "backbone")
  expect_error(gbif_name_usage(c(5231190, "9WLSS")), "col_usage")
})

test_that("the tax_* call sites resolve through the pinned wrappers", {
  # Static source scan: no call site may hit the unpinned rgbif entry points.
  # R/gbif_checklist.R is excluded: it IS the pinning layer around rgbif.
  r_files <- list.files(
    system.file("R", package = "taxinfo"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  r_files <- r_files[!grepl("gbif_checklist", r_files)]
  skip_if(
    length(r_files) == 0,
    "R/ sources not installed (running from load_all)"
  )

  calls <- unlist(lapply(r_files, readLines, warn = FALSE))
  expect_false(any(grepl("[^[]rgbif::name_backbone(_checklist)?\\(", calls)))
  expect_false(any(grepl("[^[]rgbif::name_usage\\(", calls)))
})

test_that("gbif_key_checklist tells backbone keys from COL XR keys", {
  expect_equal(gbif_key_checklist(5231190L), "backbone")
  expect_equal(gbif_key_checklist(2e6), "backbone")
  expect_equal(gbif_key_checklist(c("5231190", NA)), "backbone")
  expect_equal(gbif_key_checklist("Q2M4"), "colxr")
  expect_equal(gbif_key_checklist(c("5TYZ9", "9WLSS")), "colxr")
})

test_that("gbif_occ_checklist_args only pins keys outside rgbif's default", {
  local_mocked_bindings(gbif_ambient_checklist = function() "backbone")
  with_ck <- function(taxonKey, checklistKey = NULL) NULL
  without_ck <- function(taxonKey) NULL

  expect_equal(gbif_occ_checklist_args(5231190, with_ck), list())
  expect_equal(
    gbif_occ_checklist_args("Q2M4", with_ck),
    list(checklistKey = gbif_checklist_key("colxr"))
  )
  expect_error(gbif_occ_checklist_args("Q2M4", without_ck), "rgbif")

  local_mocked_bindings(gbif_ambient_checklist = function() "colxr")
  expect_equal(gbif_occ_checklist_args("Q2M4", with_ck), list())
  expect_equal(
    gbif_occ_checklist_args(5231190, with_ck),
    list(checklistKey = gbif_checklist_key("backbone"))
  )
})

test_that("gbif_occ_count sends the checklistKey of the key to occ_search", {
  local_mocked_bindings(gbif_ambient_checklist = function() "backbone")
  seen <- NULL
  local_mocked_bindings(
    occ_search = function(..., checklistKey = NULL) {
      seen <<- c(list(...), list(checklistKey = checklistKey))
      list(meta = list(count = 42))
    },
    occ_count = function(...) {
      seen <<- c(list(...), list(via = "occ_count"))
      7
    },
    .package = "rgbif"
  )
  expect_equal(gbif_occ_count("5TYZ9", hasCoordinate = TRUE), 42)
  expect_equal(seen$checklistKey, gbif_checklist_key("colxr"))
  expect_equal(seen$limit, 0)
  expect_true(seen$hasCoordinate)

  expect_equal(gbif_occ_count(8168319, hasCoordinate = TRUE), 7)
  expect_equal(seen$via, "occ_count")
  expect_equal(seen$taxonKey, 8168319)
})

test_that("gbif_taxa_to_backbone re-matches COL XR taxa by name", {
  local_mocked_bindings(
    gbif_backbone_checklist = function(name_data, checklist = NULL, ...) {
      expect_equal(checklist, "backbone")
      tibble::tibble(
        verbatim_name = c("Amanita muscaria", "Xxx yyy"),
        usageKey = c(8168319L, NA),
        matchType = c("EXACT", "NONE")
      )
    }
  )
  col_taxa <- tibble::tibble(
    usageKey = c("5TYZ9", "ZZZZ"),
    canonicalName = c("Amanita muscaria", "Xxx yyy"),
    verbatim_name = c("Amanita muscaria", "Xxx yyy")
  )
  expect_message(
    res <- gbif_taxa_to_backbone(col_taxa, verbose = FALSE),
    "Xxx yyy"
  )
  expect_equal(res$usageKey, 8168319L)
  expect_equal(res$verbatim_name, "Amanita muscaria")

  bb_taxa <- tibble::tibble(usageKey = 8168319L, verbatim_name = "A b")
  expect_identical(gbif_taxa_to_backbone(bb_taxa, verbose = FALSE), bb_taxa)
})

test_that("download_sql queries backbone keys but reports COL XR keys", {
  fixture_path <- testthat::test_path("fixtures", "gbif_simple_csv_sample.rds")
  skip_if_not(file.exists(fixture_path), "SIMPLE_CSV fixture not generated")
  occ <- readRDS(fixture_path)
  sp_key <- as.integer(
    names(sort(table(occ$speciesKey), decreasing = TRUE))[1]
  )
  sent_sql <- NULL
  local_mocked_bindings(
    gbif_backbone_checklist = function(name_data, checklist = NULL, ...) {
      tibble::tibble(
        verbatim_name = "Xylobolus subpileatus",
        usageKey = sp_key,
        matchType = "EXACT"
      )
    },
    gbif_download = function(..., sql = NULL, verbose = TRUE) {
      sent_sql <<- sql
      occ
    }
  )
  col_taxa <- tibble::tibble(
    usageKey = "COLKEY",
    canonicalName = "Xylobolus subpileatus",
    verbatim_name = "Xylobolus subpileatus"
  )
  res <- gbif_occur_coords_download(
    col_taxa,
    n_occur = 1e6,
    keep_cols = c("decimalLongitude", "decimalLatitude"),
    method = "download_sql",
    country = "FR",
    verbose = FALSE
  )
  expect_match(sent_sql, as.character(sp_key), fixed = TRUE)
  expect_false(grepl("COLKEY", sent_sql, fixed = TRUE))
  expect_gt(nrow(res), 0)
  expect_true(all(res$usageKey == "COLKEY"))
})

test_that("records of a COL XR query are attributed by name", {
  fixture_path <- testthat::test_path("fixtures", "gbif_simple_csv_sample.rds")
  skip_if_not(file.exists(fixture_path), "SIMPLE_CSV fixture not generated")
  occ <- readRDS(fixture_path)
  col_taxa <- tibble::tibble(
    usageKey = "COLKEY",
    canonicalName = "Xylobolus subpileatus",
    verbatim_name = "Xylobolus subpileatus"
  )
  res <- attribute_gbif_records(occ, col_taxa)
  expect_gt(nrow(res), 0)
  expect_true(all(res$usageKey == "COLKEY"))
})

test_that("gbif_match_taxa keeps its columns when no name matches", {
  local_mocked_bindings(
    gbif_backbone_checklist = function(name_data, checklist = NULL, ...) {
      tibble::tibble(
        matchType = "NONE",
        confidence = 100L,
        verbatim_name = name_data
      )
    }
  )
  res <- gbif_match_taxa(c("Xxx yyy", "Zzz www"))
  expect_equal(nrow(res), 0)
  expect_named(res, c("usageKey", "canonicalName", "verbatim_name"))
})

test_that("gbif_match_taxa drops unmatched names of a mixed call", {
  local_mocked_bindings(
    gbif_backbone_checklist = function(name_data, checklist = NULL, ...) {
      tibble::tibble(
        usageKey = c("8168319", NA),
        canonicalName = c("Amanita muscaria", NA),
        matchType = c("EXACT", "NONE"),
        verbatim_name = name_data
      )
    }
  )
  res <- gbif_match_taxa(c("Amanita muscaria", "Xxx yyy"))
  expect_equal(res$verbatim_name, "Amanita muscaria")
  expect_named(res, c("usageKey", "canonicalName", "verbatim_name"))
})

test_that("gbif_key_chr writes numeric keys in full", {
  expect_equal(
    gbif_key_chr(c(2e6, 5231190, 3e9, NA)),
    c("2000000", "5231190", "3000000000", NA)
  )
  expect_equal(gbif_key_chr(c(2000000L, NA)), c("2000000", NA))
  expect_equal(gbif_key_chr(c("5TYZ9", NA)), c("5TYZ9", NA))
})

test_that("attribute_gbif_records matches round double keys to integer keys", {
  occ <- data.frame(
    speciesKey = c(2000000L, 3000000L),
    taxonKey = c(2000000L, 3000000L),
    species = c("A b", "C d")
  )
  gbif_taxa <- tibble::tibble(
    usageKey = 2e6,
    canonicalName = "Not the species column",
    verbatim_name = "A b"
  )
  res <- attribute_gbif_records(occ, gbif_taxa)
  expect_equal(nrow(res), 1)
  expect_equal(res$species, "A b")
})
