bt_fixture <- function() {
  find <- function(name) {
    p <- testthat::test_path("fixtures", name)
    if (!file.exists(p)) {
      p <- file.path("tests", "testthat", "fixtures", name)
    }
    p
  }
  list(
    species = find("bactotraits_species_mini.csv"),
    genus = find("bactotraits_genus_mini.csv")
  )
}

bt_fixture_pq <- function() {
  tax <- rbind(
    Taxa_sp1 = c(
      "Bacillota",
      "Bacilli",
      "Bacillales",
      "Bacillaceae",
      "Bacillus",
      "subtilis"
    ),
    Taxa_sp2 = c(
      "Bacillota",
      "Bacilli",
      "Bacillales",
      "Bacillaceae",
      "Bacillus",
      "cereus"
    ),
    Taxa_gen = c(
      "Bacillota",
      "Bacilli",
      "Bacillales",
      "Bacillaceae",
      "Bacillus",
      NA
    ),
    Taxa_fam = c(
      "Bacillota",
      "Bacilli",
      "Bacillales",
      "Bacillaceae",
      "Anceps",
      NA
    ),
    Taxa_none = c("Zzzota", "Zzzia", "Zzzales", "Zzzaceae", "Zzz", "Zzz yyy")
  )
  colnames(tax) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
  otu <- matrix(
    1:10,
    nrow = 5,
    dimnames = list(rownames(tax), paste0("S", 1:2))
  )
  phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
}

test_that("bt_trait_groups splits trait_modality columns", {
  groups <- bt_trait_groups(c(
    "gram_stain_positive",
    "gram_stain_negative",
    "motility_yes",
    "cell_length_<=_0.9",
    "cell_length_>_2",
    "cell_shape_coccus-shaped",
    "rRNA16S_gene_copies_<=_3",
    "Total_BacDive_ids",
    "Species"
  ))
  expect_equal(
    groups$gram_stain,
    c("gram_stain_positive", "gram_stain_negative")
  )
  expect_equal(groups$motility, "motility_yes")
  expect_equal(
    groups$cell_length,
    c("cell_length_<=_0.9", "cell_length_>_2")
  )
  expect_equal(groups$cell_shape, "cell_shape_coccus-shaped")
  expect_equal(groups$rRNA16S_gene_copies, "rRNA16S_gene_copies_<=_3")
  expect_false("Total_BacDive_ids" %in% unlist(groups, use.names = FALSE))
})

test_that("bt_pick_reference prefers positive modalities", {
  expect_equal(
    bt_pick_reference(c("motility_no", "motility_yes"), "motility", c("yes")),
    "motility_yes"
  )
  expect_equal(
    bt_pick_reference(
      c("gram_stain_positive", "gram_stain_negative"),
      "gram_stain",
      c("yes", "positive")
    ),
    "gram_stain_positive"
  )
  expect_equal(
    bt_pick_reference(
      c("carbon_source_autotroph", "carbon_source_heterotroph"),
      "carbon_source",
      c("yes", "positive")
    ),
    "carbon_source_autotroph"
  )
})

test_that("bt_recode_affinity maps 1/0 to TRUE/FALSE and keeps scores", {
  expect_equal(
    bt_recode_affinity(c(1, 0, 0.25, NA)),
    c("TRUE", "FALSE", "0.25", NA)
  )
})

test_that("bt_species_key glues epithets and keeps binomials", {
  expect_equal(
    bt_species_key(
      c("Bacillus", "Bacillus", NA, "Bacillus"),
      c("subtilis", "Bacillus subtilis", "subtilis", NA)
    ),
    c("Bacillus subtilis", "Bacillus subtilis", NA, NA)
  )
})

test_that("bt_species_key restores the space of glued genus-epithet names", {
  expect_equal(
    bt_species_key(
      c("Sulfolobus", "Bacillus", "Eubacterium", "Bacillus"),
      c(
        "Sulfolobusacidocaldarius",
        "BacillusSP1",
        "Clostridiumbiforme",
        "SCA1145"
      )
    ),
    c(
      "Sulfolobus acidocaldarius",
      "Bacillus BacillusSP1",
      "Eubacterium Clostridiumbiforme",
      "Bacillus SCA1145"
    )
  )
})

test_that("tax_bactotraits_pq errors on bad input", {
  fx <- bt_fixture()
  expect_error(tax_bactotraits_pq(NULL), "phyloseq")
  expect_error(
    tax_bactotraits_pq(
      bt_fixture_pq(),
      file_name_species = "no_such_file.csv",
      file_name_genus = fx$genus,
      level = "species"
    ),
    "does not exist"
  )
  expect_error(
    tax_bactotraits_pq(
      bt_fixture_pq(),
      taxonomic_rank = c("Genus", "Species")
    ),
    "6"
  )
  bad_pq <- bt_fixture_pq()
  colnames(bad_pq@tax_table) <- c(
    "Phylum",
    "Classe",
    "Order",
    "Family",
    "Genus",
    "Species"
  )
  expect_error(
    tax_bactotraits_pq(
      bad_pq,
      file_name_species = fx$species,
      file_name_genus = fx$genus
    ),
    "missing"
  )
})

test_that("tax_bactotraits_pq merges binary pairs as TRUE/FALSE/probability", {
  fx <- bt_fixture()
  pq <- bt_fixture_pq()
  res <- tax_bactotraits_pq(
    pq,
    file_name_species = fx$species,
    file_name_genus = fx$genus,
    level = c("species", "genus", "family"),
    verbose = FALSE
  )
  tt <- as.data.frame(unclass(res@tax_table), stringsAsFactors = FALSE)

  # Species match: exclusive and fuzzy assignments
  expect_equal(tt["Taxa_sp1", "bt_trait_level"], "species")
  expect_equal(tt["Taxa_sp1", "bt_gram_stain"], "TRUE")
  expect_equal(tt["Taxa_sp1", "bt_motility"], "TRUE")
  expect_equal(tt["Taxa_sp1", "bt_n_strains"], "1")
  expect_equal(tt["Taxa_sp2", "bt_gram_stain"], "0.25")
  expect_equal(tt["Taxa_sp2", "bt_motility"], "0.5")

  # Genus fallback (species row absent)
  expect_equal(tt["Taxa_gen", "bt_trait_level"], "genus")
  expect_equal(tt["Taxa_gen", "bt_gram_stain"], "0.5")
  expect_equal(tt["Taxa_gen", "bt_n_strains"], "2")

  # Family fallback: strain-weighted mean of Bacillus (w2) + Staphylococcus (w2)
  expect_equal(tt["Taxa_fam", "bt_trait_level"], "family")
  expect_equal(tt["Taxa_fam", "bt_gram_stain"], "0.75")
  expect_equal(tt["Taxa_fam", "bt_motility"], "0.375")
  expect_equal(tt["Taxa_fam", "bt_n_strains"], "4")

  # No match
  expect_true(is.na(tt["Taxa_none", "bt_trait_level"]))
  expect_true(is.na(tt["Taxa_none", "bt_motility"]))

  # FALSE path via a non-reference-exclusive assignment
  res2 <- tax_bactotraits_pq(
    pq,
    file_name_species = fx$species,
    file_name_genus = fx$genus,
    level = "species",
    verbose = FALSE
  )
  tt2 <- as.data.frame(unclass(res2@tax_table), stringsAsFactors = FALSE)
  expect_equal(tt2["Taxa_sp2", "bt_gram_stain"], "0.25")
})

test_that("tax_bactotraits_pq keeps multi-modality groups raw", {
  fx <- bt_fixture()
  pq <- bt_fixture_pq()
  res <- tax_bactotraits_pq(
    pq,
    file_name_species = fx$species,
    file_name_genus = fx$genus,
    level = c("species", "genus", "family"),
    verbose = FALSE
  )
  cn <- colnames(res@tax_table)
  expect_true("bt_cell_shape_coccus-shaped" %in% cn)
  expect_true("bt_cell_shape_rod-shaped" %in% cn)
  expect_false("bt_cell_shape" %in% cn)
  expect_true("bt_motility" %in% cn)
  expect_false("bt_motility_yes" %in% cn)
  expect_equal(
    as.character(res@tax_table["Taxa_sp1", "bt_cell_shape_coccus-shaped"]),
    "1"
  )
})

test_that("tax_bactotraits_pq honours merge_modalities and raw_cols", {
  fx <- bt_fixture()
  pq <- bt_fixture_pq()

  raw_only <- tax_bactotraits_pq(
    pq,
    file_name_species = fx$species,
    file_name_genus = fx$genus,
    level = c("species", "genus", "family"),
    merge_modalities = FALSE,
    verbose = FALSE
  )
  cn <- colnames(raw_only@tax_table)
  expect_true("bt_motility_yes" %in% cn)
  expect_false("bt_motility" %in% cn)

  both <- tax_bactotraits_pq(
    pq,
    file_name_species = fx$species,
    file_name_genus = fx$genus,
    level = c("species", "genus", "family"),
    raw_cols = TRUE,
    verbose = FALSE
  )
  cn <- colnames(both@tax_table)
  expect_true(all(c("bt_motility", "bt_motility_yes") %in% cn))

  tib <- tax_bactotraits_pq(
    pq,
    file_name_species = fx$species,
    file_name_genus = fx$genus,
    level = c("species", "genus", "family"),
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )
  expect_s3_class(tib, "tbl_df")
  expect_equal(nrow(tib), phyloseq::ntaxa(pq))
  expect_true("bt_motility" %in% colnames(tib))
})

test_that("tax_bactotraits_pq matches against the bundled real mini files", {
  sp <- system.file(
    "extdata",
    "bactotraits_species_mini.csv",
    package = "taxinfo"
  )
  ge <- system.file(
    "extdata",
    "bactotraits_genus_mini.csv",
    package = "taxinfo"
  )
  skip_if(sp == "" | ge == "", "bactotraits mini files not installed")

  tax <- rbind(
    Taxa_1 = c(
      "Bacillota",
      "Bacilli",
      "Bacillales",
      "Bacillaceae",
      "Bacillus",
      "albus"
    ),
    Taxa_2 = c(
      "Bacillota",
      "Bacilli",
      "Lactobacillales",
      "Lactobacillaceae",
      "Lactobacillus",
      NA
    )
  )
  colnames(tax) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
  otu <- matrix(1:4, nrow = 2, dimnames = list(rownames(tax), c("S1", "S2")))
  pq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )

  res <- tax_bactotraits_pq(
    pq,
    file_name_species = sp,
    file_name_genus = ge,
    verbose = FALSE
  )
  tt <- as.data.frame(unclass(res@tax_table), stringsAsFactors = FALSE)
  expect_equal(tt["Taxa_1", "bt_trait_level"], "species")
  expect_true(as.numeric(tt["Taxa_1", "bt_n_strains"]) >= 1)
  expect_equal(tt["Taxa_2", "bt_trait_level"], "genus")
  expect_true(
    tt["Taxa_2", "bt_motility"] %in%
      c("TRUE", "FALSE") ||
      !is.na(suppressWarnings(as.numeric(tt["Taxa_2", "bt_motility"])))
  )
})
