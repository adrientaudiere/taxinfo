# Test tax_get_wk_info_pq function
# Examples from man page: tax_get_wk_info_pq.Rd

test_that("tax_get_wk_info_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_get_wk_info_pq(NULL))
})

# Examples from vignette: getting-started.Rmd and man page: tax_get_wk_info_pq.Rd
test_that("tax_get_wk_info_pq with taxnames returns tibble", {
  skip_on_cran()
  # Example from vignette: wiki_data <- tax_get_wk_info_pq(taxnames = taxa_to_query)
  # Fixed start_date/end_date keep the pageviews API URL stable so the
  # recorded cassette matches across runs (cf. test-tax_get_wk_pages_info.R).
  taxa_to_query <- c(
    "Amanita muscaria",
    "Boletus edulis",
    "Cantharellus cibarius"
  )
  vcr::use_cassette("wk_info_taxnames", {
    wiki_data <- tax_get_wk_info_pq(
      taxnames = taxa_to_query,
      time_to_sleep = 0,
      start_date = "2024-01-01",
      end_date = "2024-01-31"
    )
  })

  expect_s3_class(wiki_data, "tbl_df")
  expect_true(nrow(wiki_data) > 0)
})

test_that("tax_get_wk_info_pq returns phyloseq with add_to_phyloseq = TRUE", {
  skip_on_cran()
  # Example: data_fungi_mini_cleanNames_wk_info <- tax_get_wk_info_pq(data_fungi_mini_cleanNames)
  vcr::use_cassette("wk_info_phyloseq", {
    wk_info <- tax_get_wk_info_pq(
      load_clean_pq(),
      time_to_sleep = 0,
      start_date = "2024-01-01",
      end_date = "2024-01-31"
    )
  })

  expect_s4_class(wk_info, "phyloseq")
})

test_that("tax_get_wk_info_pq returns tibble with add_to_phyloseq = FALSE", {
  skip_on_cran()
  # Example: wk_info <- tax_get_wk_info_pq(subset_taxa_pq(...))
  vcr::use_cassette("wk_info_tibble", {
    wk_info <- tax_get_wk_info_pq(
      load_clean_pq(),
      add_to_phyloseq = FALSE,
      time_to_sleep = 0,
      start_date = "2024-01-01",
      end_date = "2024-01-31"
    )
  })

  expect_s3_class(wk_info, "tbl_df")
})
