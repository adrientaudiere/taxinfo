# A tiny synthetic fungal phyloseq with taxa whose spore-volume match is known
# (values cross-checked against the bundled database), so the test is fully
# offline and deterministic.
make_spore_pq <- function() {
  tax <- rbind(
    Taxa_1 = c(
      "Fungi",
      "Basidiomycota",
      "Agaricomycetes",
      "Russulales",
      "Stereaceae",
      "Stereum",
      "ostrea"
    ),
    Taxa_2 = c(
      "Fungi",
      "Basidiomycota",
      "Agaricomycetes",
      "Polyporales",
      "Steccherinaceae",
      "Antrodiella",
      "sp1"
    ),
    Taxa_3 = c(
      "Plantae",
      "Streptophyta",
      "PLANT",
      "PLANT",
      "PLANT",
      "Stereum",
      "ostrea"
    ),
    Taxa_4 = c(
      "Fungi",
      "JUNK",
      "JUNK",
      "JUNK",
      "JUNKXYZ",
      "Notagenus",
      "notaspecies"
    )
  )
  colnames(tax) <- c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species"
  )
  otu <- matrix(
    c(5, 1, 0, 2, 3, 4, 1, 0),
    nrow = 4,
    dimnames = list(rownames(tax), c("S1", "S2"))
  )
  phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
}

test_that("tax_spores_volume_pq matches at species, genus, and family levels", {
  ps <- make_spore_pq()
  res <- tax_spores_volume_pq(ps, verbose = FALSE)

  tt <- as.data.frame(unclass(res@tax_table), stringsAsFactors = FALSE)
  lvl <- stats::setNames(tt$spore_meiospores_matching_level, rownames(tt))
  vol <- stats::setNames(
    as.numeric(tt$spore_meiospores_volume),
    rownames(tt)
  )

  # Stereum ostrea -> exact species match
  expect_equal(lvl[["Taxa_1"]], "species")
  expect_equal(unname(vol[["Taxa_1"]]), 25.91814, tolerance = 1e-4)

  # Antrodiella sp1 -> genus-level geometric mean
  expect_equal(lvl[["Taxa_2"]], "genus")
  expect_equal(unname(vol[["Taxa_2"]]), 10.07861, tolerance = 1e-4)
})

test_that("tax_spores_volume_pq applies the fungal-kingdom guard", {
  ps <- make_spore_pq()
  res <- tax_spores_volume_pq(ps, verbose = FALSE)
  tt <- as.data.frame(unclass(res@tax_table), stringsAsFactors = FALSE)
  lvl <- stats::setNames(tt$spore_meiospores_matching_level, rownames(tt))

  # A non-fungal taxon with a fungal genus name must not match.
  expect_true(is.na(lvl["Taxa_3"]))
  # A fungal taxon absent from the database stays unmatched.
  expect_true(is.na(lvl["Taxa_4"]))
})

test_that("tax_spores_volume_pq can return a tibble and honours col_prefix", {
  ps <- make_spore_pq()
  tib <- tax_spores_volume_pq(
    ps,
    spore_types = "Meiospores",
    metrics = "SporeVolume",
    col_prefix = "sp_",
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )
  expect_s3_class(tib, "tbl_df")
  expect_equal(nrow(tib), 4L)
  expect_true(all(
    c("sp_meiospores_volume", "sp_meiospores_matching_level") %in% colnames(tib)
  ))
})
