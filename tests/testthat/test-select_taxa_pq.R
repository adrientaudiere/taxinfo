# Test select_taxa_pq function
# Examples from man page: select_taxa_pq.Rd

test_that("select_taxa_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(select_taxa_pq(NULL))
})

test_that("select_taxa_pq aborts clearly when no taxa match", {
  expect_error(
    select_taxa_pq(
      data_fungi,
      taxonomic_rank = c("Genus", "Species"),
      taxnames = "Definitely Notataxon",
      verbose = FALSE
    ),
    "No taxa match"
  )
})

# Examples from man page: select_taxa_pq.Rd (lines 40-54)
test_that("select_taxa_pq selects taxa by currentCanonicalSimple", {
  # Example: select_taxa_pq(data_fungi_mini_cleanNames, taxonomic_rank = "currentCanonicalSimple",
  #   taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"), verbose = FALSE, clean_pq = FALSE)
  data_fungi_mini_cleanNames <- gna_verifier_pq(
    data_fungi_mini,
    data_sources = 210
  )
  result <- select_taxa_pq(
    data_fungi_mini_cleanNames,
    taxonomic_rank = "currentCanonicalSimple",
    taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
    verbose = FALSE,
    clean_pq = FALSE
  )
  expect_s4_class(result, "phyloseq")
  # Result should have fewer taxa than original
  expect_true(ntaxa(result) <= ntaxa(data_fungi_mini_cleanNames))
})

test_that("select_taxa_pq selects taxa by multiple columns", {
  # Example: select_taxa_pq(data_fungi, taxonomic_rank = c("Genus", "Species"),
  #   taxnames = c("Xylodon flaviporus"), verbose = FALSE, clean_pq = FALSE)
  result <- select_taxa_pq(
    data_fungi,
    taxonomic_rank = c("Genus", "Species"),
    taxnames = c("Xylodon flaviporus"),
    verbose = FALSE,
    clean_pq = FALSE
  )
  expect_s4_class(result, "phyloseq")
})

test_that("select_taxa_pq selects taxa by Trait column", {
  # Example: select_taxa_pq(data_fungi, taxonomic_rank = "Trait", taxnames = c("Soft Rot"))
  expect_warning(expect_warning(expect_warning(result <- select_taxa_pq(
    data_fungi,
    taxonomic_rank = "Trait",
    taxnames = c("Soft Rot")
  ))))
  expect_s4_class(result, "phyloseq")
})
