# Test tax_get_wk_lang function
# Examples from man page: tax_get_wk_lang.Rd

# Examples from man page: tax_get_wk_lang.Rd (lines 31-36)
test_that("tax_get_wk_lang returns tibble structure", {
  skip_on_cran()
  # Example: tax_get_wk_lang("Q10723171")
  vcr::use_cassette("wk_lang_structure", {
    result <- tax_get_wk_lang("Q10723171")
  })

  expect_s3_class(result, "tbl_df")

  # Should have expected columns
  expect_true("title" %in% colnames(result))
  expect_true("site" %in% colnames(result))
  expect_true("lang" %in% colnames(result))
})

test_that("tax_get_wk_lang returns data for valid taxon_id", {
  skip_on_cran()
  # Example: tax_get_wk_lang("Q10723171") |> nrow()
  vcr::use_cassette("wk_lang_valid", {
    result <- tax_get_wk_lang("Q10723171")
  })

  # Should return at least some rows for a known taxon
  expect_true(nrow(result) > 0)
})

test_that("tax_get_wk_lang handles NA taxon_id", {
  skip_on_cran()
  result <- tax_get_wk_lang(NA)

  expect_s3_class(result, "tbl_df")
  # Should have site as NA
  expect_true(is.na(result$site[1]))
})

test_that("tax_get_wk_lang handles empty string taxon_id", {
  skip_on_cran()
  result <- tax_get_wk_lang("")

  expect_s3_class(result, "tbl_df")
  # Should have site as NA
  expect_true(is.na(result$site[1]))
})

test_that("tax_get_wk_lang languages_pages parameter works", {
  skip_on_cran()
  # Test with specific language filter
  vcr::use_cassette("wk_lang_filter", {
    result_all <- tax_get_wk_lang("Q10723171")
    result_en <- tax_get_wk_lang("Q10723171", languages_pages = c("en"))
  })

  expect_s3_class(result_en, "tbl_df")

  # Filtered result should have fewer or equal rows
  expect_true(nrow(result_en) <= nrow(result_all))

  # If result has rows, all should be in English
  if (nrow(result_en) > 0 && !is.na(result_en$lang[1])) {
    expect_true(all(result_en$lang == "en"))
  }
})

test_that("tax_get_wk_lang handles non-existent taxon_id", {
  skip_on_cran()
  # Test with a non-existent taxon ID
  vcr::use_cassette("wk_lang_nonexistent", {
    result <- tax_get_wk_lang("Q999999999999")
  })

  expect_true(inherits(result, "tbl_df") || is.na(result))
})
