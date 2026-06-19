# Test tax_occur_check_pq function
# Examples from man page: tax_occur_check_pq.Rd

clean <- load_clean_pq()

test_that("tax_occur_check_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_occur_check_pq(NULL))
})

# Examples from man page: tax_occur_check_pq.Rd (lines 65-101)
test_that("tax_occur_check_pq returns data frame with add_to_phyloseq = FALSE", {
  skip_on_cran()
  vcr::use_cassette("occur_check_pq_df", {
    check_res <- suppressWarnings(tax_occur_check_pq(
      clean,
      longitude = 2.3,
      latitude = 48,
      radius_km = 100,
      n_occur = 5,
      method = "search",
      add_to_phyloseq = FALSE
    ))
  })

  expect_s3_class(check_res, "data.frame")
  expect_true("taxa_name" %in% colnames(check_res))
  expect_true("count_in_radius" %in% colnames(check_res))
  expect_true("total_count_in_world" %in% colnames(check_res))
})

test_that("tax_occur_check_pq returns phyloseq and respects col_prefix", {
  skip_on_cran()
  vcr::use_cassette("occur_check_pq_phyloseq", {
    res <- suppressWarnings(tax_occur_check_pq(
      clean,
      longitude = 2.3,
      latitude = 48,
      radius_km = 50,
      n_occur = 5,
      method = "search"
    ))
  })
  expect_s4_class(res, "phyloseq")
  expect_true("count_in_radius" %in% colnames(res@tax_table))

  vcr::use_cassette("occur_check_pq_prefix", {
    res_prefix <- suppressWarnings(tax_occur_check_pq(
      clean,
      longitude = 2.3,
      latitude = 48,
      radius_km = 50,
      n_occur = 5,
      method = "search",
      col_prefix = "occ_"
    ))
  })
  expect_s4_class(res_prefix, "phyloseq")
  expect_true(any(grepl("^occ_", colnames(res_prefix@tax_table))))
})

test_that("tax_occur_check_pq both physeq and taxnames errors", {
  expect_error(
    tax_occur_check_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("tax_occur_check_pq neither physeq nor taxnames errors", {
  expect_error(
    tax_occur_check_pq(physeq = NULL, taxnames = NULL),
    "You must specify either"
  )
})

test_that("tax_occur_check_pq add_to_phyloseq cannot be TRUE with taxnames", {
  expect_error(
    tax_occur_check_pq(
      taxnames = c("Amanita muscaria"),
      longitude = 2.3,
      latitude = 48,
      add_to_phyloseq = TRUE
    ),
    "cannot be TRUE when.*taxnames"
  )
})

test_that("tax_occur_check_pq requires longitude and latitude", {
  expect_error(
    tax_occur_check_pq(
      clean,
      longitude = NULL,
      latitude = 48
    )
  )

  expect_error(
    tax_occur_check_pq(
      clean,
      longitude = 2.3,
      latitude = NULL
    )
  )
})
