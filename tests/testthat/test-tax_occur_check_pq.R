# Test tax_occur_check_pq function
# Examples from man page: tax_occur_check_pq.Rd

test_that("tax_occur_check_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_occur_check_pq(NULL))
})

# Examples from man page: tax_occur_check_pq.Rd (lines 65-101)
test_that("tax_occur_check_pq returns data frame with add_to_phyloseq = FALSE", {
  # Example: check_res <- tax_occur_check_pq(data_fungi_mini_cleanNames,
  #   longitude = 2.3, latitude = 48, radius_km = 100, n_occur = 50, add_to_phyloseq = FALSE)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  check_res <- tax_occur_check_pq(
    data_fungi_mini_cleanNames,
    longitude = 2.3,
    latitude = 48,
    radius_km = 100,
    n_occur = 50,
    add_to_phyloseq = FALSE
  )

  expect_s3_class(check_res, "data.frame")
  expect_true("taxa_name" %in% colnames(check_res))
  expect_true("count_in_radius" %in% colnames(check_res))
  expect_true("total_count_in_world" %in% colnames(check_res))
})

test_that("tax_occur_check_pq returns phyloseq with add_to_phyloseq = TRUE", {
  # Example: data_fungi_mini_cleanNames_range_verif <- tax_occur_check_pq(data_fungi_mini_cleanNames,
  #   longitude = 2.3, latitude = 48, radius_km = 50, n_occur = 10)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  data_fungi_mini_cleanNames_range_verif <- tax_occur_check_pq(
    data_fungi_mini_cleanNames,
    longitude = 2.3,
    latitude = 48,
    radius_km = 50,
    n_occur = 10
  )

  expect_s4_class(data_fungi_mini_cleanNames_range_verif, "phyloseq")
  expect_true(
    "count_in_radius" %in%
      colnames(data_fungi_mini_cleanNames_range_verif@tax_table)
  )
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
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)

  expect_error(
    tax_occur_check_pq(
      data_fungi_mini_cleanNames,
      longitude = NULL,
      latitude = 48
    )
  )

  expect_error(
    tax_occur_check_pq(
      data_fungi_mini_cleanNames,
      longitude = 2.3,
      latitude = NULL
    )
  )
})

test_that("tax_occur_check_pq col_prefix parameter works", {
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  result <- tax_occur_check_pq(
    data_fungi_mini_cleanNames,
    longitude = 2.3,
    latitude = 48,
    radius_km = 50,
    n_occur = 10,
    col_prefix = "occ_"
  )

  expect_s4_class(result, "phyloseq")
  # Check that columns have the prefix
  col_names <- colnames(result@tax_table)
  expect_true(any(grepl("^occ_", col_names)))
})
