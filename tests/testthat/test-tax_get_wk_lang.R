# Test tax_get_wk_lang function
# Examples from man page: tax_get_wk_lang.Rd

# Examples from man page: tax_get_wk_lang.Rd (lines 31-36)
test_that("tax_get_wk_lang returns tibble structure", {
  # Example: tax_get_wk_lang("Q10723171")
  result <- tax_get_wk_lang("Q10723171")

  expect_s3_class(result, "tbl_df")

  # Should have expected columns
  expect_true("title" %in% colnames(result))
  expect_true("site" %in% colnames(result))
  expect_true("lang" %in% colnames(result))
})

test_that("tax_get_wk_lang returns data for valid taxon_id", {
  # Example: tax_get_wk_lang("Q10723171") |> nrow()
  result <- tax_get_wk_lang("Q10723171")

  # Should return at least some rows for a known taxon
  expect_true(nrow(result) > 0)
})
