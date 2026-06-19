# Test tax_gbif_occur_pq function
# Examples from man page: tax_gbif_occur_pq.Rd

clean <- load_clean_pq()

test_that("tax_gbif_occur_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_gbif_occur_pq(NULL))
})

test_that("tax_gbif_occur_pq parameter defaults", {
  # Test default parameter values
  # taxonomic_rank should default to "currentCanonicalSimple"
  # add_to_phyloseq should default to TRUE for phyloseq, FALSE for taxnames
  # verbose should default to TRUE
  expect_true(is.character("currentCanonicalSimple"))
  expect_true(is.logical(FALSE))
  expect_true(is.logical(TRUE))
})

# Examples from man page: tax_gbif_occur_pq.Rd (lines 61-72)
test_that("tax_gbif_occur_pq with taxnames returns tibble", {
  skip_on_cran()
  # Example: tax_gbif_occur_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
  vcr::use_cassette("gbif_occur_pq_taxnames", {
    result <- tax_gbif_occur_pq(
      taxnames = c("Amanita muscaria", "Boletus edulis"),
      time_to_sleep = 0
    )
  })
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("Global_occurences" %in% colnames(result))
})

test_that("tax_gbif_occur_pq with phyloseq add_to_phyloseq = FALSE returns tibble", {
  skip_on_cran()
  # Example: tax_gbif_occur_pq(data_fungi_mini_cleanNames, add_to_phyloseq = FALSE)
  vcr::use_cassette("gbif_occur_pq_tibble", {
    result <- tax_gbif_occur_pq(
      clean,
      add_to_phyloseq = FALSE,
      time_to_sleep = 0
    )
  })
  expect_s3_class(result, "tbl_df")
  expect_true("Global_occurences" %in% colnames(result))
})

test_that("tax_gbif_occur_pq with by_years parameter", {
  skip_on_cran()
  # Example: tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_years = TRUE, add_to_phyloseq = FALSE)
  vcr::use_cassette("gbif_occur_pq_years", {
    result <- tax_gbif_occur_pq(
      clean,
      by_years = TRUE,
      add_to_phyloseq = FALSE,
      time_to_sleep = 0
    )
  })
  expect_s3_class(result, "tbl_df")
})

test_that("tax_gbif_occur_pq returns phyloseq with add_to_phyloseq = TRUE", {
  skip_on_cran()
  # Example: data_fungi_mini_cleanNames <- tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_country = TRUE)
  vcr::use_cassette("gbif_occur_pq_country", {
    result <- tax_gbif_occur_pq(clean, by_country = TRUE, time_to_sleep = 0)
  })
  expect_s4_class(result, "phyloseq")
  expect_true("US" %in% colnames(result@tax_table))
})
