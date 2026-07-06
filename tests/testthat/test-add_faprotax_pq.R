# A tiny synthetic archaeal phyloseq with taxa whose FAPROTAX assignment is
# known, so the test is fully offline and deterministic.
make_archaea_pq <- function() {
  tax <- rbind(
    Taxa_1 = c(
      "Archaea", "Halobacteriota", "Methanomicrobia",
      "Methanomicrobiales", "Methanoregulaceae", "Methanoregula",
      "Methanoregula sp1"
    ),
    Taxa_2 = c(
      "Archaea", "Thermoproteota", "Nitrososphaeria",
      "Nitrososphaerales", "Nitrososphaeraceae", "Nitrososphaera",
      "Nitrososphaera sp1"
    ),
    Taxa_3 = c(
      "Archaea", "Thermoproteota", "Bathyarchaeia",
      "JUNK", "JUNK", "JUNKXYZ", NA
    )
  )
  colnames(tax) <- c(
    "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"
  )
  otu <- matrix(
    c(5, 1, 0, 2, 3, 4),
    nrow = 3,
    dimnames = list(rownames(tax), c("S1", "S2"))
  )
  phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
}

test_that("add_faprotax_pq errors on non-phyloseq input", {
  expect_error(add_faprotax_pq(NULL), "phyloseq")
  expect_error(add_faprotax_pq(data.frame(a = 1)), "phyloseq")
})

test_that("add_faprotax_pq assigns the expected functional groups", {
  res <- add_faprotax_pq(make_archaea_pq(), verbose = FALSE)

  expect_s4_class(res, "phyloseq")
  expect_true("faprotax_groups" %in% colnames(res@tax_table))

  groups <- as.character(res@tax_table[, "faprotax_groups"])
  names(groups) <- taxa_names(res)

  # Pinned biological truths from the bundled FAPROTAX database.
  expect_match(groups["Taxa_1"], "methanogenesis")
  expect_match(groups["Taxa_2"], "aerobic_ammonia_oxidation")
  expect_true(is.na(groups["Taxa_3"]))
})

test_that("add_faprotax_pq binary = TRUE adds per-group 0/1 columns", {
  res <- add_faprotax_pq(make_archaea_pq(), binary = TRUE, verbose = FALSE)

  expect_true("faprotax_methanogenesis" %in% colnames(res@tax_table))
  meth <- as.integer(res@tax_table[, "faprotax_methanogenesis"])
  names(meth) <- taxa_names(res)
  expect_equal(meth[["Taxa_1"]], 1L)
  expect_equal(meth[["Taxa_3"]], 0L)
})

test_that("add_faprotax_pq returns a tibble when add_to_phyloseq = FALSE", {
  res <- add_faprotax_pq(
    make_archaea_pq(),
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_true("faprotax_groups" %in% colnames(res))
})

test_that("tax_levels controls the matching scope", {
  # FAPROTAX assigns these archaea through higher-rank patterns
  # (e.g. *Methanomicrobiales*Methanoregula*), so restricting the lineage to
  # Genus + Species drops matches that the full lineage recovers.
  full <- add_faprotax_pq(make_archaea_pq(), verbose = FALSE)
  restricted <- add_faprotax_pq(
    make_archaea_pq(),
    tax_levels = c("Genus", "Species"),
    verbose = FALSE
  )
  g_full <- as.character(full@tax_table[, "faprotax_groups"])
  g_restr <- as.character(restricted@tax_table[, "faprotax_groups"])

  expect_match(g_full[1], "methanogenesis")
  expect_true(is.na(g_restr[1]))
})

test_that("re-running suffixes columns instead of duplicating them", {
  res1 <- add_faprotax_pq(make_archaea_pq(), verbose = FALSE)
  res2 <- add_faprotax_pq(res1, verbose = FALSE)

  cols <- colnames(res2@tax_table)
  expect_false(any(duplicated(cols)))
  expect_true("faprotax_groups" %in% cols)
  expect_true("faprotax_groups_1" %in% cols)
})

test_that("'_' is a word boundary, matching the official FAPROTAX tool", {
  # GTDB polyphyly suffix: *Methanobacterium* must match "Methanobacterium_B"
  # (FAPROTAX treats "_" as a boundary; PCRE's \\b would not).
  tax <- rbind(
    Taxa_1 = c(
      "Archaea", "Methanobacteriota", "Methanobacteria",
      "Methanobacteriales", "Methanobacteriaceae", "Methanobacterium_B", NA
    )
  )
  colnames(tax) <- c(
    "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"
  )
  otu <- matrix(c(3, 4), nrow = 1, dimnames = list("Taxa_1", c("S1", "S2")))
  pq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
  res <- add_faprotax_pq(pq, verbose = FALSE)
  expect_match(
    as.character(res@tax_table[, "faprotax_groups"]),
    "methanogenesis"
  )
})

test_that("subtract_group excludes chloroplasts from cyanobacteria", {
  tax <- rbind(
    cyano = c("Bacteria", "Cyanobacteria", "Cyanobacteriia", NA, NA, NA, NA),
    chloro = c("Bacteria", "Cyanobacteria", "Chloroplast", NA, NA, NA, NA)
  )
  colnames(tax) <- c(
    "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"
  )
  otu <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    dimnames = list(rownames(tax), c("S1", "S2"))
  )
  pq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
  res <- add_faprotax_pq(pq, verbose = FALSE)
  g <- as.character(res@tax_table[, "faprotax_groups"])
  names(g) <- taxa_names(res)
  expect_match(g[["cyano"]], "cyanobacteria")
  # Chloroplast is subtracted from the cyanobacteria group.
  expect_false(isTRUE(grepl("cyanobacteria", g[["chloro"]])))
})

test_that("parse_faprotax reads the bundled database", {
  file <- system.file("extdata", "FAPROTAX.txt", package = "taxinfo")
  skip_if(!nzchar(file), "FAPROTAX.txt not installed")
  groups <- parse_faprotax(file)
  expect_gt(length(groups), 50)
  expect_true("methanogenesis" %in% names(groups))
})
