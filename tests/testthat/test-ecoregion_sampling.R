# Offline saturation tests: fake fetch_page + key_fun, no GBIF, no shapefile.

fake_gbif_taxa <- function(n = 1) {
  tibble::tibble(
    usageKey = seq_len(n) * 1000,
    verbatim_name = paste("Taxon", seq_len(n))
  )
}

paged_fetch_page <- function(n_total) {
  force(n_total)
  function(taxon_key, limit, start) {
    idx <- seq(start + 1, min(start + limit, n_total))
    idx <- idx[idx <= n_total]
    if (length(idx) == 0) {
      return(tibble::tibble())
    }
    tibble::tibble(
      decimalLongitude = runif(length(idx), -180, 180),
      decimalLatitude = runif(length(idx), -90, 90),
      idx = idx
    )
  }
}

test_that("ecoregion_saturation_step tracks keys and fires on patience", {
  st <- list(n_points = 0, seen = character(), stale = 0L, saturated = FALSE)
  st <- ecoregion_saturation_step(
    st,
    c("E1", "E2"),
    10,
    min_points = 10,
    patience = 2
  )
  expect_false(st$saturated)
  expect_equal(st$n_points, 10)
  expect_equal(st$stale, 0L)

  st <- ecoregion_saturation_step(
    st,
    c("E1"),
    10,
    min_points = 10,
    patience = 2
  )
  expect_false(st$saturated)
  expect_equal(st$stale, 1L)

  st <- ecoregion_saturation_step(
    st,
    c("E3", NA, ""),
    10,
    min_points = 10,
    patience = 2
  )
  expect_false(st$saturated)
  expect_equal(st$stale, 0L)

  st <- ecoregion_saturation_step(st, "E1", 10, min_points = 10, patience = 2)
  st <- ecoregion_saturation_step(st, "E1", 10, min_points = 10, patience = 2)
  expect_true(st$saturated)
})

test_that("ecoregion_saturation_fetch stops once the ecoregion set saturates", {
  # New ecoregions only in the first 200 rows, then always "E1"
  key_fun <- function(batch) {
    ifelse(batch$idx <= 200, paste0("E", batch$idx), "E1")
  }
  res <- ecoregion_saturation_fetch(
    gbif_taxa = fake_gbif_taxa(),
    n_occur = 1000,
    batch_size = 100,
    min_points = 100,
    patience = 2,
    verbose = FALSE,
    fetch_page = paged_fetch_page(1200),
    key_fun = key_fun
  )
  sat <- attr(res, "saturation")
  expect_equal(nrow(res), 400)
  expect_equal(sat$status, "saturated")
  expect_equal(sat$n_points, 400)
  expect_equal(sat$n_ecoregions, 200)
  expect_equal(sat$n_batches, 4)
})

test_that("ecoregion_saturation_fetch respects min_points before firing", {
  key_fun <- function(batch) "E1"
  res <- ecoregion_saturation_fetch(
    gbif_taxa = fake_gbif_taxa(),
    n_occur = 1000,
    batch_size = 100,
    min_points = 250,
    patience = 2,
    verbose = FALSE,
    fetch_page = paged_fetch_page(1200),
    key_fun = key_fun
  )
  sat <- attr(res, "saturation")
  expect_equal(sat$status, "saturated")
  expect_equal(sat$n_points, 300)
})

test_that("ecoregion_saturation_fetch caps at n_occur when keys keep coming", {
  key_fun <- function(batch) paste0("E", batch$idx)
  res <- ecoregion_saturation_fetch(
    gbif_taxa = fake_gbif_taxa(),
    n_occur = 350,
    batch_size = 100,
    min_points = 100,
    patience = 2,
    verbose = FALSE,
    fetch_page = paged_fetch_page(1200),
    key_fun = key_fun
  )
  sat <- attr(res, "saturation")
  expect_equal(sat$status, "cap")
  expect_equal(sat$n_points, 350)
  expect_equal(nrow(res), 350)
})

test_that("ecoregion_saturation_fetch detects data exhaustion", {
  key_fun <- function(batch) paste0("E", batch$idx)
  res <- ecoregion_saturation_fetch(
    gbif_taxa = fake_gbif_taxa(),
    n_occur = 1000,
    batch_size = 100,
    min_points = 100,
    patience = 2,
    verbose = FALSE,
    fetch_page = paged_fetch_page(150),
    key_fun = key_fun
  )
  sat <- attr(res, "saturation")
  expect_equal(sat$status, "exhausted")
  expect_equal(sat$n_points, 150)
})

test_that("ecoregion_saturation_fetch reports missing taxa", {
  two <- fake_gbif_taxa(2)
  fetch_one <- function(taxon_key, limit, start) {
    if (taxon_key == 1000) {
      paged_fetch_page(50)(taxon_key, limit, start)
    } else {
      tibble::tibble()
    }
  }
  res <- ecoregion_saturation_fetch(
    gbif_taxa = two,
    n_occur = 1000,
    batch_size = 100,
    min_points = 100,
    patience = 2,
    verbose = FALSE,
    fetch_page = fetch_one,
    key_fun = function(batch) paste0("E", batch$idx)
  )
  expect_equal(attr(res, "missing_taxa"), "Taxon 2")
  expect_equal(unique(res$taxon_name), "Taxon 1")
  expect_true(all(res$usageKey == 1000))
})

test_that("ecoregion_saturation_fetch validates its parameters", {
  expect_error(
    ecoregion_saturation_fetch(gbif_taxa = fake_gbif_taxa(), batch_size = 0),
    "batch_size"
  )
  expect_error(
    ecoregion_saturation_fetch(gbif_taxa = fake_gbif_taxa(), min_points = -1),
    "min_points"
  )
  expect_error(
    ecoregion_saturation_fetch(gbif_taxa = fake_gbif_taxa(), patience = 0),
    "patience"
  )
  expect_error(
    ecoregion_saturation_fetch(gbif_taxa = fake_gbif_taxa(), n_occur = -5),
    "n_occur"
  )
  expect_error(
    ecoregion_saturation_fetch(),
    "taxnames"
  )
})

test_that("tax_ecoregion_occur rejects bad saturation parameters before any call", {
  expect_error(
    tax_ecoregion_occur("X y", saturation = TRUE, min_points = -1),
    "min_points"
  )
  expect_error(
    tax_ecoregion_occur("X y", saturation = TRUE, patience = 0),
    "patience"
  )
  expect_error(
    tax_ecoregion_occur("X y", saturation = TRUE, batch_size = 0),
    "batch_size"
  )
  expect_error(
    tax_check_ecoregion(
      taxnames = "X y",
      longitudes = 2,
      latitudes = 48,
      saturation = TRUE,
      min_points = -1
    ),
    "min_points"
  )
  expect_error(
    tax_ecoregion_occur_pq(
      taxnames = "X y",
      saturation = TRUE,
      patience = 0
    ),
    "patience"
  )
})

test_that("the saturation summary is where each ecoregion function documents it", {
  skip_if_not_installed("sf")
  fake_ecoregions <- sf::st_sf(
    ECO_NAME = "Box",
    WWF_MHTNAM = "Biome",
    WWF_REALM2 = "Realm",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 40),
        c(10, 40),
        c(10, 50),
        c(0, 50),
        c(0, 40)
      ))),
      crs = 4326
    )
  )
  sat <- tibble::tibble(
    taxon_name = "A b",
    n_points = 3,
    n_ecoregions = 1L,
    n_batches = 1L,
    status = "exhausted"
  )
  fake_occ <- tibble::tibble(
    taxon_name = "A b",
    usageKey = 1L,
    decimalLongitude = c(2, 3, 4),
    decimalLatitude = c(45, 46, 47)
  )
  attr(fake_occ, "missing_taxa") <- character()
  attr(fake_occ, "saturation") <- sat
  local_mocked_bindings(
    load_ecoregions = function(...) fake_ecoregions,
    ecoregion_saturation_fetch = function(...) fake_occ
  )

  res <- tax_ecoregion_occur("A b", saturation = TRUE, verbose = FALSE)
  expect_equal(attr(res, "saturation"), sat)

  res_pq <- tax_ecoregion_occur_pq(
    taxnames = "A b",
    saturation = TRUE,
    verbose = FALSE
  )
  expect_equal(attr(res_pq, "saturation"), sat)

  res_check <- tax_check_ecoregion(
    taxnames = "A b",
    longitudes = 2,
    latitudes = 45,
    saturation = TRUE,
    verbose = FALSE
  )
  expect_equal(attr(res_check$taxon_ecoregions, "saturation"), sat)
  expect_true(res_check$is_in_ecoregion["A b", "point_1"])

  empty_occ <- fake_occ[0, ]
  attr(empty_occ, "missing_taxa") <- "A b"
  attr(empty_occ, "saturation") <- sat
  local_mocked_bindings(ecoregion_saturation_fetch = function(...) empty_occ)
  res_empty <- tax_ecoregion_occur("A b", saturation = TRUE, verbose = FALSE)
  expect_equal(res_empty$n_occur, 0L)
  expect_equal(attr(res_empty, "saturation"), sat)
})

test_that("tax_ecoregion_occur keeps taxa when no name matches GBIF", {
  local_mocked_bindings(
    gbif_backbone_checklist = function(name_data, checklist = NULL, ...) {
      tibble::tibble(matchType = "NONE", verbatim_name = name_data)
    }
  )
  for (sat in c(TRUE, FALSE)) {
    res <- suppressMessages(
      tax_ecoregion_occur(
        c("Xxx yyy", "Zzz www"),
        saturation = sat,
        verbose = FALSE
      )
    )
    expect_equal(res$taxon_name, c("Xxx yyy", "Zzz www"))
    expect_equal(res$n_occur, c(0L, 0L))
    expect_true(all(is.na(res$ECO_NAME)))
  }
})

test_that("ecoregion_saturation_fetch reports unmatched names of a mixed call", {
  local_mocked_bindings(
    gbif_backbone_checklist = function(name_data, checklist = NULL, ...) {
      tibble::tibble(
        usageKey = c(1L, NA),
        canonicalName = c("A b", NA),
        matchType = c("EXACT", "NONE"),
        verbatim_name = name_data
      )
    }
  )
  occ <- ecoregion_saturation_fetch(
    taxnames = c("A b", "Xxx yyy"),
    n_occur = 100,
    batch_size = 50,
    min_points = 10,
    verbose = FALSE,
    fetch_page = function(taxon_key, limit, start) {
      tibble::tibble(
        decimalLongitude = rep(2, limit),
        decimalLatitude = rep(46, limit)
      )
    },
    key_fun = function(batch) rep("E1", nrow(batch))
  )
  expect_equal(unique(occ$taxon_name), "A b")
  expect_equal(attr(occ, "missing_taxa"), "Xxx yyy")
  expect_equal(attr(occ, "saturation")$taxon_name, "A b")
})
