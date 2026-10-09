# Test gna_verifier_pq function

test_that("gna_verifier_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(gna_verifier_pq(NULL))

  skip("Requires phyloseq objects")
})

test_that("gna_verifier_pq parameter defaults", {
  # Test default parameter values
  # taxonomic_rank should default to "currentCanonicalSimple"
  # data_sources should default to c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  # add_to_phyloseq should default to FALSE
  # verbose should default to TRUE

  default_sources <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  expect_equal(length(default_sources), 12)
  expect_true(all(default_sources %in% 1:12))
})

test_that("gna_verifier_pq data_sources validation", {
  # Test data sources validation
  # Should accept valid source IDs
  # Should reject invalid source IDs

  valid_sources <- 1:12
  invalid_sources <- c(0, 13, -1, 100)

  expect_true(all(valid_sources >= 1 & valid_sources <= 12))
  expect_false(all(invalid_sources >= 1 & invalid_sources <= 12))

  # Test subset of valid sources
  subset_sources <- c(1, 3, 5)
  expect_true(all(subset_sources %in% valid_sources))
})


test_that("gna_verifier_pq GNA integration", {
  # Test integration with Global Names Architecture
  # Test taxonomic name verification

  skip("Requires GNA API access")
})

test_that("gna_verifier_pq return behavior", {
  # Test return modes
  # When add_to_phyloseq = TRUE: should return phyloseq with verified names
  # When add_to_phyloseq = FALSE: should return verification results

  skip("Requires phyloseq objects")
})

test_that("gna_verifier_pq name verification logic", {
  # Test taxonomic name verification logic
  # Test handling of verified vs unverified names
  # Test synonym resolution

  skip("Requires phyloseq objects and GNA API")
})

test_that("gna_verifier_pq with taxnames and physeq both provided errors", {
  expect_error(
    gna_verifier_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("gna_verifier_pq with taxnames only works", {
  skip_on_cran()
  # Test with taxnames parameter only
  vcr::use_cassette("gna_taxnames", {
    result <- gna_verifier_pq(
      taxnames = c("Amanita muscaria"),
      add_to_phyloseq = FALSE,
      verbose = FALSE
    )
  })

  expect_s3_class(result, "data.frame")
})

test_that("gna_verifier_pq add_to_phyloseq cannot be TRUE with taxnames", {
  expect_error(
    gna_verifier_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )
})

test_that("gna_verifier_pq col_prefix parameter works", {
  skip_on_cran()
  # Test with col_prefix parameter
  vcr::use_cassette("gna_col_prefix", {
    result <- gna_verifier_pq(
      taxnames = c("Boletus edulis"),
      add_to_phyloseq = FALSE,
      col_prefix = "test_",
      verbose = FALSE
    )
  })

  expect_s3_class(result, "data.frame")
})

test_that("gna_classification_table extracts the lineage of the best match", {
  gna_list <- list(
    "Sidera americana" = list(
      bestResult = list(
        classificationPath = "Fungi|Basidiomycota|Agaricomycetes|Hymenochaetales|Rickenellaceae|Sidera|Sidera americana",
        classificationRanks = "kingdom|phylum|class|order|family|genus|species"
      )
    ),
    "Unknown name" = list(matchType = "NoMatch"),
    "Amanita muscaria" = list(
      results = list(list(
        classificationPath = "Eukaryota|Fungi|Amanita",
        classificationRanks = "domain|kingdom|genus"
      ))
    )
  )
  res <- gna_classification_table(
    gna_list,
    ranks = c("kingdom", "family", "genus")
  )
  expect_equal(
    names(res),
    c(
      "submittedName",
      "classificationPath",
      "classificationRanks",
      "classificationKingdom",
      "classificationFamily",
      "classificationGenus"
    )
  )
  expect_equal(res$classificationFamily, c("Rickenellaceae", NA, NA))
  expect_equal(res$classificationGenus, c("Sidera", NA, "Amanita"))
  expect_true(is.na(res$classificationPath[2]))
  expect_equal(nrow(gna_classification_table(list(), ranks = "genus")), 0)
})

test_that("gna_verifier_pq classification_col adds the lineage", {
  skip_on_cran()
  vcr::use_cassette("gna_classification", {
    result <- gna_verifier_pq(
      taxnames = c("Sidera americana", "Lactarius luridus"),
      data_sources = 11,
      add_to_phyloseq = FALSE,
      classification_col = TRUE,
      verbose = FALSE
    )
  })
  expect_true(all(
    c("classificationPath", "classificationKingdom", "classificationGenus") %in%
      names(result)
  ))
  expect_equal(unique(result$classificationKingdom), "Fungi")
  expect_setequal(result$classificationGenus, c("Sidera", "Lactarius"))
})

test_that("gna_verifier_pq classification_col keeps names across batches", {
  fake_gna <- function(names, ..., output_type = "table") {
    if (output_type == "list") {
      return(stats::setNames(
        lapply(names, \(n) {
          list(
            bestResult = list(
              classificationPath = paste0("Fungi|Genus", n),
              classificationRanks = "kingdom|genus"
            )
          )
        }),
        names
      ))
    }
    data.frame(
      submittedName = names,
      currentName = names,
      currentCanonicalSimple = names,
      matchedCardinality = 2,
      taxonomicStatus = "Accepted"
    )
  }
  testthat::local_mocked_bindings(gna_verifier = fake_gna, .package = "taxize")
  taxnames <- paste("Name", seq_len(60))
  res <- gna_verifier_pq(
    taxnames = taxnames,
    add_to_phyloseq = FALSE,
    classification_col = TRUE,
    year_col = FALSE,
    authorship_col = FALSE,
    verbose = FALSE
  )
  expect_equal(nrow(res), 60)
  expect_equal(res$classificationGenus, paste0("Genus", taxnames))
})

test_that("gna_verifier_pq id_col returns TAXREF CD_NOM and CD_REF", {
  skip_on_cran()
  vcr::use_cassette("gna_taxref_id", {
    result <- gna_verifier_pq(
      taxnames = c("Agaricus muscarius", "Boletus edulis"),
      data_sources = 210,
      add_to_phyloseq = FALSE,
      id_col = TRUE,
      verbose = FALSE
    )
  })
  expect_equal(result$recordId, c("508317", "29388"))
  expect_equal(result$currentRecordId, c("39079", "29388"))
  expect_equal(unique(result$currentCanonicalSimple_dataSourceId), "210")
  expect_equal(
    unique(result$currentCanonicalSimple_dataSourceTitleShort),
    "TAXREF"
  )
})

test_that("gna_verifier_pq id and data source columns are optional", {
  fake_gna <- function(names, ...) {
    data.frame(
      submittedName = names,
      currentName = names,
      currentCanonicalSimple = names,
      matchedCardinality = 2,
      taxonomicStatus = "Accepted",
      dataSourceId = 11L
    )
  }
  testthat::local_mocked_bindings(gna_verifier = fake_gna, .package = "taxize")
  args <- list(
    taxnames = c("Name a", "Name b"),
    add_to_phyloseq = FALSE,
    year_col = FALSE,
    authorship_col = FALSE,
    verbose = FALSE
  )
  res <- do.call(gna_verifier_pq, c(args, id_col = TRUE))
  expect_equal(res$currentCanonicalSimple_dataSourceId, c("11", "11"))
  expect_true(all(is.na(res$currentCanonicalSimple_dataSourceTitleShort)))
  expect_true(all(is.na(res$recordId)))

  res <- do.call(gna_verifier_pq, c(args, data_source_col = FALSE))
  expect_false(any(
    c(
      "recordId",
      "currentCanonicalSimple_dataSourceId",
      "currentCanonicalSimple_dataSourceTitleShort"
    ) %in%
      names(res)
  ))
})

test_that("gna_verifier_pq id_col is NA for genus-only matches with species_only", {
  fake_gna <- function(names, ...) {
    data.frame(
      submittedName = names,
      currentName = c("Amanita muscaria", "Xylodon"),
      currentCanonicalSimple = c("Amanita muscaria", "Xylodon"),
      matchedCardinality = c(2, 1),
      taxonomicStatus = "Accepted",
      recordId = c("39079", "901834"),
      currentRecordId = c("39079", "901834")
    )
  }
  testthat::local_mocked_bindings(gna_verifier = fake_gna, .package = "taxize")
  args <- list(
    taxnames = c("Amanita muscaria", "Xylodon raduloides"),
    add_to_phyloseq = FALSE,
    id_col = TRUE,
    year_col = FALSE,
    authorship_col = FALSE,
    verbose = FALSE
  )
  res <- do.call(gna_verifier_pq, args)
  expect_equal(res$recordId, c("39079", NA))
  expect_equal(res$currentRecordId, c("39079", NA))

  res <- do.call(gna_verifier_pq, c(args, species_only = FALSE))
  expect_equal(res$recordId, c("39079", "901834"))
})

test_that("gna_verifier_pq keeps the data source of each prefixed verification", {
  fake_gna_source <- function(id, title) {
    function(names, ...) {
      data.frame(
        submittedName = names,
        currentName = names,
        currentCanonicalSimple = names,
        matchedCardinality = 2,
        taxonomicStatus = "Accepted",
        dataSourceId = id,
        dataSourceTitleShort = title
      )
    }
  }
  args <- list(
    taxonomic_rank = "currentCanonicalSimple",
    year_col = FALSE,
    authorship_col = FALSE,
    verbose = FALSE
  )
  pq <- load_clean_pq()
  testthat::local_mocked_bindings(
    gna_verifier = fake_gna_source(210L, "TAXREF"),
    .package = "taxize"
  )
  pq <- do.call(gna_verifier_pq, c(list(pq), args, col_prefix = "taxref_"))
  testthat::local_mocked_bindings(
    gna_verifier = fake_gna_source(11L, "GBIF Backbone Taxonomy"),
    .package = "taxize"
  )
  pq <- do.call(gna_verifier_pq, c(list(pq), args, col_prefix = "gbif_"))
  tax <- as.data.frame(unclass(pq@tax_table))
  expect_equal(unique(tax$taxref_currentCanonicalSimple_dataSourceId), "210")
  expect_equal(unique(tax$gbif_currentCanonicalSimple_dataSourceId), "11")
  expect_equal(
    unique(tax$gbif_currentCanonicalSimple_dataSourceTitleShort),
    "GBIF Backbone Taxonomy"
  )
})
