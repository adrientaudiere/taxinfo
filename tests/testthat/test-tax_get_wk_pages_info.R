# Test tax_get_wk_pages_info (moved from test-additional_functions.R, the only
# coverage of this function). The Wikimedia pageviews API is date-windowed, so
# fixed start_date/end_date are passed to keep the recorded cassette stable
# across runs.

test_that("tax_get_wk_pages_info returns page view summaries", {
  skip_on_cran()
  # tib_list references an undefined object -> errors during argument evaluation
  # (no network), preserving the original validation check.
  expect_error(
    tax_get_wk_pages_info("Q10723171", tib_list = pages_not_defined)
  )

  start <- "2024-01-01"
  end <- "2024-01-31"
  vcr::use_cassette("wk_pages_info", {
    ti1 <- tax_get_wk_pages_info(
      "Q10723171",
      start_date = start,
      end_date = end
    )
    ti2 <- tax_get_wk_pages_info(
      "Q10723171",
      languages_pages = c("fr", "en"),
      summarize_function_length = "sum",
      start_date = start,
      end_date = end
    )
    pages_Q10723171 <- tax_get_wk_lang("Q10723171")
    ti3 <- tax_get_wk_pages_info(
      tib_list = pages_Q10723171,
      start_date = start,
      end_date = end
    )
  })

  expect_length(ti1, 2)
  expect_length(ti2, 2)
  expect_gte(ti1$page_views, ti2$page_views)
  expect_length(ti3, 2)
})
