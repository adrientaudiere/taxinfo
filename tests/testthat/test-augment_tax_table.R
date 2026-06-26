library(phyloseq)

make_ps <- function() {
  data(data_fungi_mini, package = "MiscMetabar")
  data_fungi_mini
}

test_that("augment_tax_table adds columns, preserves taxa order and names", {
  ps <- make_ps()
  key <- taxnames_from_rank(ps@tax_table, c("Genus", "Species"))
  info <- tibble::tibble(
    taxa_name = unique(key[key != ""])[1:3],
    val = c("a", "b", "c")
  )

  out <- augment_tax_table(ps, info, taxonomic_rank = c("Genus", "Species"))
  expected_n <- sum(key %in% info$taxa_name)

  expect_s4_class(out, "phyloseq")
  expect_equal(ntaxa(out), ntaxa(ps))
  expect_identical(taxa_names(out), taxa_names(ps))
  expect_true("val" %in% colnames(out@tax_table))
  expect_gte(expected_n, 1L)
  expect_equal(sum(!is.na(out@tax_table[, "val"])), expected_n)
})

test_that("augment_tax_table keep_key controls the taxa_name column", {
  ps <- make_ps()
  info <- tibble::tibble(
    taxa_name = taxnames_from_rank(ps@tax_table, "Genus")[1],
    val = "x"
  )
  with_key <- augment_tax_table(
    ps,
    info,
    taxonomic_rank = "Genus",
    keep_key = TRUE
  )
  no_key <- augment_tax_table(
    ps,
    info,
    taxonomic_rank = "Genus",
    keep_key = FALSE
  )
  expect_true("taxa_name" %in% colnames(with_key@tax_table))
  expect_false("taxa_name" %in% colnames(no_key@tax_table))
})

test_that("augment_tax_table errors on duplicated info_tbl keys", {
  ps <- make_ps()
  k <- taxnames_from_rank(ps@tax_table, "Genus")[1]
  dup <- tibble::tibble(taxa_name = c(k, k), val = c(1, 2))
  expect_error(augment_tax_table(ps, dup, taxonomic_rank = "Genus"))
})

test_that("augment_tax_table honours info_key", {
  ps <- make_ps()
  info <- tibble::tibble(
    my_query = taxnames_from_rank(ps@tax_table, "Genus")[1],
    val = "x"
  )
  out <- augment_tax_table(
    ps,
    info,
    taxonomic_rank = "Genus",
    info_key = "my_query"
  )
  expect_true("val" %in% colnames(out@tax_table))
  expect_false("my_query" %in% colnames(out@tax_table))
})

test_that("augment_tax_table collision: NULL prefix falls back to default", {
  ps <- make_ps()
  info <- tibble::tibble(
    taxa_name = taxnames_from_rank(ps@tax_table, "Genus")[1],
    Order = "x"
  )
  expect_warning(
    out <- augment_tax_table(
      ps,
      info,
      taxonomic_rank = "Genus",
      default_prefix = "z_"
    ),
    "already exist"
  )
  expect_true("z_Order" %in% colnames(out@tax_table))
})

test_that("augment_tax_table collision: explicit prefix that still clashes aborts", {
  ps <- make_ps()
  info <- tibble::tibble(
    taxa_name = taxnames_from_rank(ps@tax_table, "Genus")[1],
    der = "x"
  )
  expect_error(
    augment_tax_table(
      ps,
      info,
      taxonomic_rank = "Genus",
      col_prefix = "Or"
    ),
    "even with"
  )
})

test_that("augment_tax_table collision with no prefix and no default aborts", {
  ps <- make_ps()
  info <- tibble::tibble(
    taxa_name = taxnames_from_rank(ps@tax_table, "Genus")[1],
    Order = "x"
  )
  expect_error(
    augment_tax_table(ps, info, taxonomic_rank = "Genus"),
    "disambiguate"
  )
})
