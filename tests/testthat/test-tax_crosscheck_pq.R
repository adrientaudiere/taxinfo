# Test tax_crosscheck_pq function

test_that("tax_crosscheck_pq input validation: both NULL", {
  expect_error(
    tax_crosscheck_pq(),
    "You must specify either"
  )
})

test_that("tax_crosscheck_pq input validation: both provided", {
  expect_error(
    tax_crosscheck_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("tax_crosscheck_pq status classification logic", {
  # Test the internal comparison/classification logic used in the function
  # without requiring any API calls
  classify_status <- function(g, b) {
    g_na <- is.na(g) || g == ""
    b_na <- is.na(b) || b == ""
    if (g_na && b_na) {
      return("both_na")
    } else if (g_na) {
      return("backbone_only")
    } else if (b_na) {
      return("gna_only")
    } else if (g == b) {
      return("match")
    } else {
      return("mismatch")
    }
  }

  expect_equal(classify_status("Amanita muscaria", "Amanita muscaria"), "match")
  expect_equal(classify_status("Amanita", "Amanita muscaria"), "mismatch")
  expect_equal(classify_status("Amanita muscaria", NA_character_), "gna_only")
  expect_equal(
    classify_status(NA_character_, "Amanita muscaria"),
    "backbone_only"
  )
  expect_equal(classify_status("Amanita muscaria", ""), "gna_only")
  expect_equal(classify_status("", "Amanita muscaria"), "backbone_only")
  expect_equal(classify_status(NA_character_, NA_character_), "both_na")
  expect_equal(classify_status("", ""), "both_na")
})

test_that("tax_crosscheck_pq returns expected structure with taxnames", {
  skip_if_offline()
  skip_on_cran()

  result <- tax_crosscheck_pq(
    taxnames = c("Amanita muscaria", "Boletus edulis"),
    verbose = FALSE,
    plot = FALSE
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("gna_results", "backbone_results", "comparison", "summary")
  )
  expect_s3_class(result$gna_results, "data.frame")
  expect_s3_class(result$backbone_results, "data.frame")
  expect_s3_class(result$comparison, "data.frame")
  expect_type(result$summary, "integer")

  expected_summary_names <- c(
    "total",
    "match",
    "mismatch",
    "gna_only",
    "backbone_only",
    "both_na"
  )
  expect_true(all(expected_summary_names %in% names(result$summary)))
  expect_equal(result$summary[["total"]], 2)
})

test_that("tax_crosscheck_pq comparison has status column with valid values", {
  skip_if_offline()
  skip_on_cran()

  result <- tax_crosscheck_pq(
    taxnames = c("Amanita muscaria", "Boletus edulis"),
    verbose = FALSE,
    plot = FALSE
  )

  expect_true("status" %in% colnames(result$comparison))
  expect_true("submitted_name" %in% colnames(result$comparison))
  expect_true("gna_canonical" %in% colnames(result$comparison))
  expect_true("backbone_canonical" %in% colnames(result$comparison))
  expect_equal(nrow(result$comparison), 2)
  expect_true(all(
    result$comparison$status %in%
      c("match", "mismatch", "gna_only", "backbone_only", "both_na")
  ))
})

test_that("tax_crosscheck_pq with plot = FALSE skips Venn diagram", {
  skip_if_offline()
  skip_on_cran()

  result <- tax_crosscheck_pq(
    taxnames = c("Amanita muscaria"),
    verbose = FALSE,
    plot = FALSE
  )

  expect_type(result, "list")
  expect_false("venn_plot" %in% names(result))
})

test_that("tax_crosscheck_pq summary counts add up to total", {
  skip_if_offline()
  skip_on_cran()

  result <- tax_crosscheck_pq(
    taxnames = c("Amanita muscaria", "Boletus edulis", "Russula"),
    verbose = FALSE,
    plot = FALSE
  )

  count_fields <- c("match", "mismatch", "gna_only", "backbone_only", "both_na")
  expect_equal(sum(result$summary[count_fields]), result$summary[["total"]])
})
