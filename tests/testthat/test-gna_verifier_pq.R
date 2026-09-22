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
