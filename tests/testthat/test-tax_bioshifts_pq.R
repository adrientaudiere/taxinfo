test_that("bioshift_norm_name lower-cases and collapses separators", {
  expect_equal(
    bioshift_norm_name(c("Genusa  speciesa", "GENUSB_speciesB", "")),
    c("genusa speciesa", "genusb speciesb", NA)
  )
})

make_toy_pq <- function() {
  otu <- matrix(
    1,
    nrow = 3,
    ncol = 2,
    dimnames = list(c("t1", "t2", "t3"), c("s1", "s2"))
  )
  tax <- matrix(
    c(
      "Genusa",
      "speciesa",
      "Genusb",
      "speciesb",
      "Genusc",
      "speciesc"
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("t1", "t2", "t3"), c("Genus", "Species"))
  )
  phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
}

test_that("tax_bioshifts_pq attaches summarised shift rates from shifts_data", {
  pq <- make_toy_pq()
  shifts <- data.frame(
    sp_name_checked = c(
      "Genusa speciesa",
      "Genusa speciesa",
      "Genusb speciesb"
    ),
    type = c("LAT", "LAT", "ELE"),
    calc_rate = c(1.2, 0.8, -3.5),
    stringsAsFactors = FALSE
  )
  res <- tax_bioshifts_pq(
    pq,
    taxonomic_rank = c("Genus", "Species"),
    shifts_data = shifts,
    skip_name_verification = TRUE,
    add_to_phyloseq = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(
    c("bioshift_LAT_rate", "bioshift_ELE_rate", "bioshift_n_records") %in%
      colnames(res)
  ))
  # t1 = Genusa speciesa: mean(1.2, 0.8) = 1.0 LAT, no ELE, 2 records
  expect_equal(res$bioshift_LAT_rate[1], 1.0)
  expect_true(is.na(res$bioshift_ELE_rate[1]))
  expect_equal(res$bioshift_n_records[1], 2L)
  # t2 = Genusb speciesb: ELE -3.5, no LAT, 1 record
  expect_equal(res$bioshift_ELE_rate[2], -3.5)
  expect_true(is.na(res$bioshift_LAT_rate[2]))
  expect_equal(res$bioshift_n_records[2], 1L)
  # t3 unmatched
  expect_equal(res$bioshift_n_records[3], 0L)
})

test_that("tax_bioshifts_pq matches a single underscore binomial column", {
  otu <- matrix(1, nrow = 2, ncol = 1, dimnames = list(c("t1", "t2"), "s1"))
  tax <- matrix(
    c("Genusa_speciesa", "Genusb_speciesb"),
    nrow = 2,
    dimnames = list(c("t1", "t2"), "Genus_species")
  )
  pq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
  shifts <- data.frame(
    sp_name_checked = "Genusa speciesa",
    type = "LAT",
    calc_rate = 2.0,
    stringsAsFactors = FALSE
  )
  res <- tax_bioshifts_pq(
    pq,
    taxonomic_rank = "Genus_species",
    shifts_data = shifts,
    skip_name_verification = TRUE,
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )
  expect_equal(res$bioshift_LAT_rate[1], 2.0)
  expect_equal(res$bioshift_n_records[1], 1L)
  expect_equal(res$bioshift_n_records[2], 0L)
})

test_that("tax_bioshifts_pq (default) harmonises BioShifts names via GNA", {
  # tax_table carries GNA-verified canonical names; BioShifts ships a synonym.
  otu <- matrix(1, nrow = 2, ncol = 1, dimnames = list(c("t1", "t2"), "s1"))
  tax <- matrix(
    c("Aaa bbb", "Ccc ddd"),
    nrow = 2,
    dimnames = list(c("t1", "t2"), "currentCanonicalSimple")
  )
  pq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax)
  )
  shifts <- data.frame(
    sp_name_checked = "Aaa bbb var. syn",
    type = "LAT",
    calc_rate = 4.0,
    stringsAsFactors = FALSE
  )
  # Mock the verifier: the BioShifts synonym resolves to the accepted "Aaa bbb".
  testthat::local_mocked_bindings(
    gna_verifier_pq = function(taxnames, ...) {
      tibble::tibble(
        submittedName = taxnames,
        currentCanonicalSimple = rep("Aaa bbb", length(taxnames)),
        taxa_names_in_phyloseq = taxnames
      )
    }
  )
  res <- tax_bioshifts_pq(
    pq,
    taxonomic_rank = "currentCanonicalSimple",
    shifts_data = shifts,
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )
  # t1 = "Aaa bbb" gets the shift via the harmonised synonym; t2 stays unmatched.
  expect_equal(res$bioshift_LAT_rate[1], 4.0)
  expect_equal(res$bioshift_n_records[1], 1L)
  expect_equal(res$bioshift_n_records[2], 0L)
})

test_that("tax_bioshifts_pq returns a phyloseq when add_to_phyloseq = TRUE", {
  pq <- make_toy_pq()
  shifts <- data.frame(
    sp_name_checked = "Genusa speciesa",
    type = "LAT",
    calc_rate = 2.0,
    stringsAsFactors = FALSE
  )
  res <- tax_bioshifts_pq(
    pq,
    taxonomic_rank = c("Genus", "Species"),
    shifts_data = shifts,
    skip_name_verification = TRUE,
    verbose = FALSE
  )
  expect_s4_class(res, "phyloseq")
  expect_true("bioshift_LAT_rate" %in% colnames(res@tax_table))
})

test_that("tax_bioshifts_pq validates inputs", {
  pq <- make_toy_pq()
  expect_error(
    tax_bioshifts_pq(
      pq,
      taxonomic_rank = "Nope",
      shifts_data = data.frame(),
      skip_name_verification = TRUE
    ),
    "not found"
  )
  bad <- data.frame(foo = 1)
  expect_error(
    tax_bioshifts_pq(
      pq,
      taxonomic_rank = c("Genus", "Species"),
      shifts_data = bad,
      skip_name_verification = TRUE
    ),
    "missing required"
  )
  expect_error(tax_bioshifts_pq(NULL), "phyloseq")
})
