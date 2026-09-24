fe_write_fixture <- function() {
  fe_df <- data.frame(
    Species = c("Foo bar", "Foo bar", "Foo bar", NA, "Baz qux"),
    Genus = c("Foo", "Foo", "Foo", "Foo", "Baz"),
    Organ = c("Root", "Root", "Leaf", "Stem", "Root"),
    Study_Type = c(
      "Culture based study",
      "Microbiome study",
      "Culture based study",
      "Microbiome study",
      "Culture based study"
    ),
    Ref_Type = c(
      "Literature",
      "Literature",
      "GenBank",
      "Literature",
      "Literature"
    ),
    Reference = c("Ref1", "Ref1", "Ref2", "Ref3", "Ref4"),
    DOI = c("doi1", "doi1", "doi2", NA, "doi4"),
    Accession = c("A1", "A1", "A2", NA, "A4"),
    Country = c("France", "France", "Italy", "Spain", "France"),
    Host_Genus = c("Quercus", "Quercus", "Pinus", "Betula", NA),
    stringsAsFactors = FALSE
  )
  f <- tempfile(fileext = ".csv")
  utils::write.csv(fe_df, f, row.names = FALSE)
  f
}

test_that("tax_funendo_pq errors on bad input", {
  fixture <- fe_write_fixture()
  expect_error(tax_funendo_pq(), "physeq")
  expect_error(
    tax_funendo_pq(taxnames = "Foo", file_name = "no_such_file.csv"),
    "does not exist"
  )
  expect_error(
    tax_funendo_pq(taxnames = "Foo", physeq = "x", file_name = fixture),
    "not both"
  )
  bad <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1), bad, row.names = FALSE)
  expect_error(
    tax_funendo_pq(taxnames = "Foo", file_name = bad, verbose = FALSE),
    "missing"
  )
})

test_that("tax_funendo_pq aggregates species matches", {
  fixture <- fe_write_fixture()
  res <- tax_funendo_pq(
    taxnames = "Foo bar",
    file_name = fixture,
    verbose = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(res$taxa_name, "Foo bar")
  expect_equal(res$fe_match_level, "species")
  expect_equal(res$fe_n_records, "3")
  expect_equal(res$fe_n_references, "2")
  expect_equal(res$fe_n_accessions, "2")
  expect_equal(res$fe_organs, "Root | Leaf")
  expect_equal(res$fe_countries, "France | Italy")
  expect_equal(res$fe_host_genera, "Quercus | Pinus")
})

test_that("tax_funendo_pq falls back to genus level", {
  fixture <- fe_write_fixture()

  genus_only <- tax_funendo_pq(
    taxnames = "Foo",
    file_name = fixture,
    verbose = FALSE
  )
  expect_equal(genus_only$fe_match_level, "genus")
  expect_equal(genus_only$fe_n_records, "4")
  expect_equal(genus_only$fe_organs, "Root | Leaf | Stem")

  unmatched_sp <- tax_funendo_pq(
    taxnames = "Foo zzz",
    file_name = fixture,
    verbose = FALSE
  )
  expect_equal(unmatched_sp$fe_match_level, "genus")
  expect_equal(unmatched_sp$fe_n_records, "4")

  no_match <- tax_funendo_pq(
    taxnames = "Zzz yyy",
    file_name = fixture,
    verbose = FALSE
  )
  expect_true(is.na(no_match$fe_match_level))
  expect_true(is.na(no_match$fe_n_records))
})

test_that("tax_funendo_pq honours match_level and max_levels", {
  fixture <- fe_write_fixture()

  sp_only <- tax_funendo_pq(
    taxnames = "Foo",
    file_name = fixture,
    match_level = "species",
    verbose = FALSE
  )
  expect_true(is.na(sp_only$fe_match_level))

  truncated <- tax_funendo_pq(
    taxnames = "Foo",
    file_name = fixture,
    max_levels = 1,
    verbose = FALSE
  )
  expect_equal(truncated$fe_organs, "Root | (+2 more)")
})

test_that("tax_funendo_pq adds columns to a phyloseq object", {
  fixture <- fe_write_fixture()
  # phyloseq-side Species holds the epithet only (as in data_fungi_mini):
  # taxonomic_rank = c("Genus", "Species") then builds "Foo bar" / "Zzz yyy"
  tax <- rbind(
    Taxa_1 = c("Fungi", "Foo", "bar"),
    Taxa_2 = c("Fungi", "Zzz", "yyy")
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

  res <- tax_funendo_pq(
    pq,
    taxonomic_rank = c("Genus", "Species"),
    file_name = fixture,
    verbose = FALSE
  )
  expect_s4_class(res, "phyloseq")
  expect_true(all(
    c("fe_match_level", "fe_n_records", "fe_organs") %in%
      colnames(res@tax_table)
  ))
  expect_equal(
    as.character(res@tax_table["Taxa_1", "fe_match_level"]),
    "species"
  )
  expect_true(is.na(res@tax_table["Taxa_2", "fe_match_level"]))
  expect_equal(phyloseq::taxa_names(res), c("Taxa_1", "Taxa_2"))

  tib <- tax_funendo_pq(
    pq,
    taxonomic_rank = c("Genus", "Species"),
    file_name = fixture,
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )
  expect_s3_class(tib, "tbl_df")
  expect_true("fe_n_records" %in% colnames(tib))

  # Re-running with a colliding prefix is a hard error
  expect_error(
    tax_funendo_pq(
      res,
      taxonomic_rank = c("Genus", "Species"),
      file_name = fixture,
      verbose = FALSE
    ),
    "already exist"
  )
})

test_that("tax_funendo_pq matches against the bundled mini file", {
  mini <- system.file("extdata", "funendo_mini.csv", package = "taxinfo")
  skip_if(mini == "", "funendo_mini.csv not installed")

  res <- tax_funendo_pq(
    taxnames = "Trametes versicolor",
    file_name = mini,
    verbose = FALSE
  )
  expect_equal(res$fe_match_level, "species")
  expect_true(as.numeric(res$fe_n_records) >= 1)
  expect_false(is.na(res$fe_organs))

  # "Stereum ostrea" is absent from FunEndo (accepted name differs) but the
  # Stereum genus is present: the genus fallback must kick in
  fallback <- tax_funendo_pq(
    taxnames = "Stereum ostrea",
    file_name = mini,
    verbose = FALSE
  )
  expect_equal(fallback$fe_match_level, "genus")
  expect_true(as.numeric(fallback$fe_n_records) >= 1)
})

test_that("fe_reference_key merges DOI spellings and splits placeholders", {
  doi <- c(
    "https://doi.org/10.1016/J.FUNBIO.2015.09.013",
    "http://dx.doi.org/10.1016%2Fj.funbio.2015.09.013",
    "doi.10.1016/j.funbio.2015.09.013.",
    "https://doi.org/10.1016/j.x.1https://doi.org/10.1016/j.x.1",
    "Unpublished",
    "unpublished",
    NA
  )
  reference <- c(
    "Paper A",
    "Paper A (variant)",
    "Paper A",
    "Paper X",
    "Endophytes  of Pistacia",
    "Endophytes of Quercus",
    "Endophytes of pistacia"
  )
  key <- fe_reference_key(doi, reference)
  expect_equal(key[1:3], rep("10.1016/j.funbio.2015.09.013", 3))
  expect_equal(key[4], "10.1016/j.x.1")
  expect_equal(
    key[5:7],
    c(
      "endophytes of pistacia",
      "endophytes of quercus",
      "endophytes of pistacia"
    )
  )
})

test_that("tax_funendo_pq removes exact duplicate records", {
  fixture <- fe_write_fixture()
  fe_df <- utils::read.csv(fixture, colClasses = "character")
  dup_file <- tempfile(fileext = ".csv")
  utils::write.csv(rbind(fe_df, fe_df[3, ]), dup_file, row.names = FALSE)

  res <- tax_funendo_pq(
    taxnames = "Foo bar",
    file_name = dup_file,
    verbose = FALSE
  )
  expect_equal(res$fe_n_records, "3")
})

test_that("tax_funendo_pq counts one reference per unpublished title", {
  fe_df <- utils::read.csv(fe_write_fixture(), colClasses = "character")
  fe_df$DOI[1:3] <- "Unpublished"
  fe_df$Reference[1:3] <- c("Title 1", "Title 2", "Title 3")
  f <- tempfile(fileext = ".csv")
  utils::write.csv(fe_df, f, row.names = FALSE)

  res <- tax_funendo_pq(taxnames = "Foo bar", file_name = f, verbose = FALSE)
  expect_equal(res$fe_n_references, "3")
})
