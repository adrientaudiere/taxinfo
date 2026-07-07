test_that("mt_clean_genus strips GTDB prefixes and blanks", {
  expect_equal(
    mt_clean_genus(c(" g__Methanoregula", "g__BOG-1369", "?", NA, "")),
    c("Methanoregula", "BOG-1369", NA, NA, NA)
  )
})

test_that("mt_clean_species reconstructs '<genus> <epithet>' keys", {
  genus <- c("BOG-1369", "BOG-1369", "Methanocella_A", "Methanoperedens")
  species <- c(
    "s__BOG-1369 sp003164815", # GTDB spacing
    "BOG-1369sp003164815", # simplify_taxo concatenation
    "Methanocella_A_arvoryzae", # polyphyly suffix + underscore
    "Methanoperedenssp026552855"
  )
  expect_equal(
    mt_clean_species(genus, species),
    c(
      "BOG-1369 sp003164815",
      "BOG-1369 sp003164815",
      "Methanocella_A arvoryzae",
      "Methanoperedens sp026552855"
    )
  )
})

test_that("mt_clean_species handles missing genus and blanks", {
  expect_equal(
    mt_clean_species(c(NA, "Foo"), c("Bar_baz", "?")),
    c("Bar baz", NA)
  )
})

test_that("add_metatraits_pq errors on non-phyloseq input", {
  expect_error(add_metatraits_pq(NULL), "phyloseq")
})

test_that("add_metatraits_pq downloads and annotates (network, slow)", {
  skip_on_cran()
  skip_if_offline()

  tax <- rbind(
    Taxa_1 = c("Archaea", "Methanoregula", "Methanoregula sp1"),
    Taxa_2 = c("Archaea", "Nitrososphaera", "Nitrososphaera sp1")
  )
  colnames(tax) <- c("Kingdom", "Genus", "Species")
  otu <- matrix(
    c(5, 1, 2, 3),
    nrow = 2,
    dimnames = list(rownames(tax), c("S1", "S2"))
  )
  pq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )

  res <- add_metatraits_pq(pq, level = "genus", verbose = FALSE)
  expect_s4_class(res, "phyloseq")
  expect_true("mt_trait_level" %in% colnames(res@tax_table))
  # Methanoregula is a named GTDB genus present in metaTraits.
  lev <- as.character(res@tax_table[, "mt_trait_level"])
  names(lev) <- taxa_names(res)
  expect_equal(lev[["Taxa_1"]], "genus")
})
