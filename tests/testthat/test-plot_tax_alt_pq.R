alt_pq <- function() {
  pq <- load_clean_pq()
  phyloseq::sample_data(pq)$altitude <- seq(100, 800, by = 100)
  pq
}

occ_alt_fixture <- tibble::tibble(
  canonicalName = c(
    rep("Fomes fomentarius", 12),
    rep("Mycena renati", 12),
    "Xylodon flaviporus"
  ),
  elevation = c(rep(c(400, 600), 6), rep(c(100, 200, 300), 4), 50)
)

test_that("tax_alt_ses_pq computes a z-score per taxonomic name", {
  res <- tax_alt_ses_pq(alt_pq(), "altitude", occ_alt_fixture)
  fomes <- res[res$taxa_name == "Fomes fomentarius", ]
  expect_equal(fomes$n_samples, 2)
  expect_equal(fomes$sample_alt_mean, 750)
  expect_equal(fomes$gbif_alt_mean, 500)
  expect_equal(fomes$ses, 250 / sd(rep(c(400, 600), 6)))
  expect_true(is.na(res$ses[res$taxa_name == "Xylodon flaviporus"]))
  expect_true(is.na(res$gbif_alt_mean[res$taxa_name == "Basidiodendron eyrei"]))
  expect_equal(res$ses, sort(res$ses, decreasing = TRUE, na.last = TRUE))
})

test_that("tax_alt_ses_pq accepts the summary output of tax_gbif_alt", {
  summary_alt <- tibble::tibble(
    canonicalName = "Fomes fomentarius",
    altitude_mean = 500,
    altitude_sd = 100,
    altitude_n_records = 30
  )
  res <- tax_alt_ses_pq(alt_pq(), "altitude", summary_alt)
  expect_equal(res$ses[res$taxa_name == "Fomes fomentarius"], 2.5)
})

test_that("tax_alt_ses_pq validates its inputs", {
  expect_error(
    tax_alt_ses_pq(alt_pq(), "nope", occ_alt_fixture),
    "not a column"
  )
  expect_error(
    tax_alt_ses_pq(load_clean_pq(), "Tree_name", occ_alt_fixture),
    "no numeric value"
  )
  expect_error(
    tax_alt_ses_pq(alt_pq(), "altitude", data.frame(x = 1)),
    "canonicalName"
  )
  expect_error(
    tax_alt_ses_pq(
      alt_pq(),
      "altitude",
      data.frame(canonicalName = "Fomes fomentarius")
    ),
    "altitude_mean"
  )
})

test_that("plot_tax_alt_pq returns a ggplot with violins and sample lines", {
  p <- plot_tax_alt_pq(alt_pq(), "altitude", occ_alt_fixture)
  expect_s3_class(p, "ggplot")
  expect_equal(
    levels(p$data$taxa_name),
    c("Mycena renati", "Fomes fomentarius")
  )
  layers <- unname(vapply(p$layers, \(l) class(l$geom)[1], character(1)))
  expect_equal(
    layers,
    c("GeomPoint", "GeomViolin", "GeomErrorbar", "GeomText")
  )
  expect_equal(nrow(p$layers[[3]]$data), 4)
  expect_no_error(ggplot2::ggplot_build(p))

  p2 <- plot_tax_alt_pq(
    alt_pq(),
    "altitude",
    occ_alt_fixture,
    show_ses = FALSE,
    order_by_ses = FALSE,
    points_jitter = FALSE
  )
  expect_length(p2$layers, 2)
})

test_that("plot_tax_alt_pq errors without plottable taxa", {
  expect_error(
    plot_tax_alt_pq(alt_pq(), "altitude", occ_alt_fixture, min_records = 100),
    "No taxon"
  )
  expect_error(
    plot_tax_alt_pq(
      alt_pq(),
      "altitude",
      data.frame(canonicalName = "a", altitude_mean = 1)
    ),
    "one row per occurrence"
  )
})

test_that("tax_gbif_alt return_raw gives an empty long tibble without match", {
  testthat::local_mocked_bindings(
    gbif_match_taxa = function(...) {
      tibble::tibble(
        usageKey = character(),
        canonicalName = character(),
        verbatim_name = character()
      )
    }
  )
  res <- tax_gbif_alt(
    taxnames = "Fomes fomentarius",
    return_raw = TRUE,
    verbose = FALSE
  )
  expect_equal(names(res), c("canonicalName", "elevation"))
  expect_equal(nrow(res), 0)
})

test_that("tax_gbif_alt return_raw gives one row per occurrence altitude", {
  occ <- readRDS(test_path("fixtures", "gbif_simple_csv_sample.rds"))
  testthat::local_mocked_bindings(
    gbif_match_taxa = function(...) {
      tibble::tibble(
        usageKey = "2553185",
        canonicalName = "Xylobolus subpileatus",
        verbatim_name = "Xylobolus subpileatus"
      )
    },
    gbif_download = function(...) occ
  )
  res <- tax_gbif_alt(
    taxnames = "Xylobolus subpileatus",
    return_raw = TRUE,
    verbose = FALSE
  )
  expect_equal(nrow(res), sum(!is.na(occ$elevation)))
  expect_equal(unique(res$canonicalName), "Xylobolus subpileatus")
  expect_equal(sort(res$elevation), sort(occ$elevation[!is.na(occ$elevation)]))
})
