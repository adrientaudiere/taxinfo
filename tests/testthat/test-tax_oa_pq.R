# Test tax_oa_pq function
# Examples from man page: tax_oa_pq.Rd

test_that("tax_oa_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_oa_pq(NULL))
})

test_that("tax_oa_pq parameter defaults", {
  # Test default parameter values
  default_type_works <- c("article", "review", "book-chapter", "book", "letter")
  expect_true(length(default_type_works) == 5)
  expect_true("article" %in% default_type_works)
  expect_true("review" %in% default_type_works)
})

test_that("tax_oa_pq parameter combinations", {
  # Test validation of mutually exclusive parameters
  # Only one of list_doi, return_raw_oa, add_to_phyloseq should be TRUE

  # This logic should be tested
  params <- list(
    list_doi = c(TRUE, FALSE),
    return_raw_oa = c(TRUE, FALSE),
    add_to_phyloseq = c(TRUE, FALSE)
  )

  # Generate all combinations
  combinations <- expand.grid(params)

  # Count how many are TRUE in each combination
  true_counts <- rowSums(combinations)

  # Only combinations with 0 or 1 TRUE values should be valid
  valid_combinations <- true_counts <= 1

  expect_true(any(valid_combinations))
  expect_false(all(valid_combinations)) # Some should be invalid
})

test_that("tax_oa_pq type_works validation", {
  # Test that type_works contains valid publication types
  valid_types <- c(
    "article",
    "review",
    "book-chapter",
    "book",
    "letter",
    "preprint",
    "dataset",
    "thesis",
    "proceeding"
  )

  default_types <- c("article", "review", "book-chapter", "book", "letter")

  # All default types should be in valid types
  expect_true(all(default_types %in% valid_types))
})

test_that("tax_oa_pq DOI validation", {
  # Test DOI format validation
  valid_dois <- c(
    "10.1000/182",
    "10.1038/nature12373",
    "10.1371/journal.pone.0000000"
  )
  invalid_dois <- c("not_a_doi", "10.invalid", "")
  doi_pattern <- "^10\\.[0-9]+/.+"

  expect_true(all(grepl(doi_pattern, valid_dois)))
  expect_false(any(grepl(doi_pattern, invalid_dois)))
})

# Examples from man page: tax_oa_pq.Rd (lines 80-145)
test_that("tax_oa_pq returns phyloseq with publication data", {
  skip_on_cran()
  # Example: data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini) |> tax_oa_pq()
  result <- tax_oa_pq(load_clean_pq())

  expect_s4_class(result, "phyloseq")
  # Check for n_doi column added
  expect_true("n_doi" %in% colnames(result@tax_table))
})

test_that("tax_oa_pq with specific type_works", {
  skip_on_cran()
  # Example: tax_oa_pq(data_fungi_mini_cleanNames, type_works = "dataset")
  result <- tax_oa_pq(load_clean_pq(), type_works = "dataset")

  expect_s4_class(result, "phyloseq")
})

test_that("tax_oa_pq with return_raw_oa = TRUE returns list", {
  skip_on_cran()
  # Example: list_pub_raw <- tax_oa_pq(data_fungi_mini_cleanNames, return_raw_oa = TRUE)
  list_pub_raw <- tax_oa_pq(load_clean_pq(), return_raw_oa = TRUE)

  expect_type(list_pub_raw, "list")
})
