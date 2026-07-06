make_harm_pq <- function(tax) {
  otu <- matrix(
    seq_len(nrow(tax) * 2),
    nrow = nrow(tax),
    dimnames = list(rownames(tax), c("s1", "s2"))
  )
  phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
}

backbone <- data.frame(
  name = c("Amanita", "Boletus", "Amanita muscaria"),
  Kingdom = "Fungi",
  Class = "Agaricomycetes",
  Order = c("Agaricales", "Boletales", "Agaricales"),
  Family = c("Amanitaceae", "Boletaceae", "Amanitaceae"),
  Genus = c("Amanita", "Boletus", "Amanita"),
  stringsAsFactors = FALSE
)

test_that("anchor = Genus overwrites every rank above the anchor", {
  tax <- matrix(
    c(
      "Fungi",
      "WrongClass",
      "WrongOrder",
      "WrongFamily",
      "Amanita",
      "Fungi",
      NA,
      NA,
      NA,
      "Boletus"
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("ASV1", "ASV2"),
      c("Kingdom", "Class", "Order", "Family", "Genus")
    )
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "Genus",
    backbone = backbone,
    verbose = FALSE
  )
  th <- as.data.frame(unclass(phyloseq::tax_table(h)))

  expect_equal(th["ASV1", "Class"], "Agaricomycetes")
  expect_equal(th["ASV1", "Order"], "Agaricales")
  expect_equal(th["ASV1", "Family"], "Amanitaceae")
  # ASV2 had NA higher ranks -> they are filled in.
  expect_equal(th["ASV2", "Order"], "Boletales")
  # The anchor rank itself is untouched.
  expect_equal(th["ASV1", "Genus"], "Amanita")
})

test_that("anchor rank and lower ranks are never modified", {
  tax <- matrix(
    c("Fungi", "WrongClass", "Amanita", "muscaria"),
    nrow = 1,
    dimnames = list("ASV1", c("Kingdom", "Class", "Genus", "Species"))
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "Genus",
    backbone = backbone,
    verbose = FALSE
  )
  th <- as.data.frame(unclass(phyloseq::tax_table(h)))
  # Species is below the Genus anchor -> untouched.
  expect_equal(th["ASV1", "Species"], "muscaria")
  expect_equal(th["ASV1", "Class"], "Agaricomycetes")
})

test_that("last_assigned anchors on the deepest rank and builds a binomial", {
  tax <- matrix(
    c(
      "Fungi",
      "WrongClass",
      "WrongOrder",
      "WrongFamily",
      "Amanita",
      "muscaria",
      "Fungi",
      NA,
      NA,
      NA,
      "Boletus",
      NA
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("ASV1", "ASV2"),
      c("Kingdom", "Class", "Order", "Family", "Genus", "Species")
    )
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "last_assigned",
    backbone = backbone,
    verbose = FALSE
  )
  th <- as.data.frame(unclass(phyloseq::tax_table(h)))
  # ASV1 deepest = Species -> queried as "Amanita muscaria" -> resolves.
  expect_equal(th["ASV1", "Family"], "Amanitaceae")
  # ASV2 deepest = Genus -> resolves on "Boletus".
  expect_equal(th["ASV2", "Family"], "Boletaceae")
})

test_that("two database tracks are harmonised via suffixes", {
  tax <- matrix(
    c(
      "Fungi",
      "WrongC",
      "Amanita",
      "Fungi",
      "OtherWrongC",
      "Amanita"
    ),
    nrow = 1,
    dimnames = list(
      "ASV1",
      c("Kingdom", "Class", "Genus", "Kingdom_Euk", "Class_Euk", "Genus_Euk")
    )
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "Genus",
    suffixes = c("", "_Euk"),
    backbone = backbone,
    verbose = FALSE
  )
  th <- as.data.frame(unclass(phyloseq::tax_table(h)))
  # Both tracks now carry the same backbone Class -> comparable.
  expect_equal(th["ASV1", "Class"], "Agaricomycetes")
  expect_equal(th["ASV1", "Class_Euk"], "Agaricomycetes")
})

test_that("keep_original preserves overwritten values", {
  tax <- matrix(
    c("Fungi", "WrongClass", "Amanita"),
    nrow = 1,
    dimnames = list("ASV1", c("Kingdom", "Class", "Genus"))
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "Genus",
    backbone = backbone,
    keep_original = TRUE,
    verbose = FALSE
  )
  th <- as.data.frame(unclass(phyloseq::tax_table(h)))
  expect_equal(th["ASV1", "Class"], "Agaricomycetes")
  expect_equal(th["ASV1", "Class_orig"], "WrongClass")
})

test_that("unmatched anchors keep their original higher ranks", {
  tax <- matrix(
    c("Fungi", "KeepClass", "Unknownus"),
    nrow = 1,
    dimnames = list("ASV1", c("Kingdom", "Class", "Genus"))
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "Genus",
    backbone = backbone,
    verbose = FALSE
  )
  th <- as.data.frame(unclass(phyloseq::tax_table(h)))
  expect_equal(th["ASV1", "Class"], "KeepClass")
})

test_that("taxa order and names are preserved", {
  tax <- matrix(
    c(
      "Fungi",
      "W",
      "Boletus",
      "Fungi",
      "W",
      "Amanita"
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("ASVb", "ASVa"), c("Kingdom", "Class", "Genus"))
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "Genus",
    backbone = backbone,
    verbose = FALSE
  )
  expect_equal(phyloseq::taxa_names(h), c("ASVb", "ASVa"))
})

test_that("harmonize_pick_candidate keeps the accepted candidate at the rank", {
  # Mimics rgbif::name_backbone("Boletus", verbose = TRUE): a HIGHERRANK best
  # match, the accepted fungal genus, and a same-rank synonym pointing to the
  # WRONG family (Morchellaceae) that must not be chosen.
  alts <- data.frame(
    rank = c("KINGDOM", "GENUS", "GENUS"),
    matchType = c("HIGHERRANK", "EXACT", "EXACT"),
    confidence = c(96, 96, 96),
    status = c("ACCEPTED", "ACCEPTED", "SYNONYM"),
    kingdom = "Fungi",
    family = c(NA, "Boletaceae", "Morchellaceae"),
    stringsAsFactors = FALSE
  )
  pick <- harmonize_pick_candidate(
    alts,
    want_rank = "genus",
    kingdom = "Fungi",
    min_confidence = 80,
    match_types = "EXACT"
  )
  expect_equal(pick$family, "Boletaceae")

  # No candidate at the requested rank -> NULL.
  expect_null(
    harmonize_pick_candidate(
      alts[alts$rank == "KINGDOM", ],
      want_rank = "genus",
      kingdom = "Fungi",
      min_confidence = 80,
      match_types = "EXACT"
    )
  )
  # Wrong kingdom filtered out -> NULL.
  expect_null(
    harmonize_pick_candidate(
      alts,
      want_rank = "genus",
      kingdom = "Animalia",
      min_confidence = 80,
      match_types = "EXACT"
    )
  )
})

test_that("resolve_ambiguous recovers via mocked verbose alternatives", {
  tax <- matrix(
    c("Fungi", "WrongClass", "Boletus"),
    nrow = 1,
    dimnames = list("ASV1", c("Kingdom", "Class", "Genus"))
  )
  fake_alts <- data.frame(
    rank = c("KINGDOM", "GENUS"),
    matchType = c("HIGHERRANK", "EXACT"),
    confidence = c(96, 96),
    status = c("ACCEPTED", "ACCEPTED"),
    kingdom = "Fungi",
    class = c(NA, "Agaricomycetes"),
    stringsAsFactors = FALSE
  )
  fake_checklist <- data.frame(
    matchType = "HIGHERRANK",
    confidence = 96,
    kingdom = "Fungi",
    class = NA_character_,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    name_backbone_checklist = function(...) fake_checklist,
    name_backbone = function(...) fake_alts,
    .package = "rgbif"
  )
  h <- tax_harmonize_backbone_pq(
    make_harm_pq(tax),
    anchor = "Genus",
    kingdom = "Fungi",
    resolve_ambiguous = TRUE,
    verbose = FALSE
  )
  th <- as.data.frame(unclass(phyloseq::tax_table(h)))
  expect_equal(th["ASV1", "Class"], "Agaricomycetes")
})

test_that("tax_harmonize_backbone_pq validates its inputs", {
  tax <- matrix(
    c("Fungi", "Amanita"),
    nrow = 1,
    dimnames = list("ASV1", c("Kingdom", "Genus"))
  )
  pq <- make_harm_pq(tax)
  expect_error(
    tax_harmonize_backbone_pq(pq, anchor = "NotARank", backbone = backbone),
    "anchor"
  )
  expect_error(
    tax_harmonize_backbone_pq(
      pq,
      anchor = "Genus",
      backbone = data.frame(Genus = "Amanita")
    ),
    "name"
  )
})
