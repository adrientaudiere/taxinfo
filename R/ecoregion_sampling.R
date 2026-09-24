#' Saturation early-stop machinery for ecoregion profiling
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Internal helpers behind the `saturation` parameter of
#' [tax_ecoregion_occur()], [tax_ecoregion_occur_pq()] and
#' [tax_check_ecoregion()]. Occurrences are fetched from GBIF in batches and,
#' after each batch, the points are mapped to WWF/TNC ecoregions. As soon as a
#' taxon's *ecoregion set* stops growing for `patience` consecutive batches
#' (and at least `min_points` points were fetched), fetching stops: the
#' ecoregion profile of the taxon has stabilised, so the remaining GBIF
#' records add no information for these functions and are not downloaded.
#'
#' The stopping rule is a pure state machine ([ecoregion_saturation_step()])
#' over sets of grouping keys; the paging loop
#' ([ecoregion_saturation_fetch()]) accepts injected `fetch_page` /
#' `key_fun` / `gbif_taxa` arguments so that both layers can be tested
#' without network access or the ecoregion shapefile.
#'
#' @keywords internal
#' @name ecoregion_sampling
NULL

#' One transition of the ecoregion-saturation state machine
#'
#' @param state A list with elements `n_points` (numeric), `seen` (character
#'  vector of grouping keys already observed) and `stale` (number of
#'  consecutive batches that brought no new key).
#' @param batch_keys (character) Grouping keys (e.g. `ECO_NAME` values) of the
#'  new batch. `NA` and empty values are ignored.
#' @param n_batch_points (numeric) Number of points in the new batch.
#' @param min_points (numeric) Minimum number of points before the rule is
#'  allowed to fire.
#' @param patience (numeric) Number of consecutive key-less batches tolerated
#'  before the set is declared saturated.
#'
#' @returns The updated `state`, with a new logical element `saturated`.
#'
#' @author Adrien Taudiere
#' @keywords internal
ecoregion_saturation_step <- function(
  state,
  batch_keys,
  n_batch_points,
  min_points,
  patience
) {
  batch_keys <- batch_keys[!is.na(batch_keys) & nzchar(batch_keys)]
  new_keys <- setdiff(batch_keys, state$seen)
  state$seen <- union(state$seen, batch_keys)
  state$n_points <- state$n_points + n_batch_points
  state$stale <- if (length(new_keys) == 0) state$stale + 1L else 0L
  state$saturated <- state$n_points >= min_points && state$stale >= patience
  state
}

#' Default `fetch_page`: one `occ_search` page for a single taxon key
#'
#' @param taxon_key A GBIF usage key.
#' @param limit (numeric) Page size.
#' @param start (numeric) Offset of the first record of the page.
#' @param time_to_sleep (numeric) Seconds to sleep before the call (GBIF
#'  rate-limiting courtesy delay).
#' @returns A tibble with `decimalLongitude`, `decimalLatitude` and, when
#'  available, `countryCode`, `year` and `gbifID`.
#' @author Adrien Taudiere
#' @keywords internal
gbif_occ_search_page <- function(
  taxon_key,
  limit,
  start,
  time_to_sleep = 0.3
) {
  Sys.sleep(time_to_sleep)
  args <- c(
    list(
      taxonKey = taxon_key,
      limit = limit,
      start = start,
      hasCoordinate = TRUE,
      hasGeospatialIssue = FALSE
    ),
    gbif_occ_checklist_args(taxon_key)
  )
  res <- do.call(rgbif::occ_search, args)$data
  if (is.null(res) || nrow(res) == 0) {
    return(tibble::tibble())
  }
  res |>
    dplyr::select(dplyr::any_of(c(
      "decimalLongitude",
      "decimalLatitude",
      "countryCode",
      "year",
      "gbifID"
    )))
}

#' Fetch GBIF occurrences per taxon until its ecoregion set saturates
#'
#' @param taxnames (character) Query names. Ignored when `gbif_taxa` is
#'  supplied.
#' @param gbif_taxa (tibble or `NULL`) Resolved GBIF taxa with columns
#'  `usageKey` and `verbatim_name` (as returned by
#'  [rgbif::name_backbone_checklist()]). When `NULL`, `taxnames` are resolved
#'  through the GBIF backbone (network).
#' @param n_occur (numeric) Hard cap on the number of occurrences fetched per
#'  taxon.
#' @param batch_size (numeric) Page size of each `fetch_page` call.
#' @param min_points (numeric) Minimum number of points fetched per taxon
#'  before the saturation rule may fire.
#' @param patience (numeric) Consecutive batches without a new ecoregion
#'  tolerated before stopping.
#' @param clean_coord (logical) If `TRUE`, run
#'  [CoordinateCleaner::clean_coordinates()] on the result.
#' @param time_to_sleep (numeric) Courtesy delay between GBIF calls (used by
#'  the default `fetch_page`).
#' @param verbose (logical) Progress messages.
#' @param fetch_page (function or `NULL`) `function(taxon_key, limit, start)`
#'  returning one page of occurrences. Defaults to [gbif_occ_search_page()].
#' @param key_fun (function or `NULL`) `function(batch)` returning the
#'  grouping keys (ecoregion names) of one batch of points. Defaults to a
#'  WWF/TNC lookup through [points_to_ecoregions()].
#'
#' @returns A tibble of occurrences with `taxon_name` and `usageKey`, with
#'  `attr(, "missing_taxa")` (query names without any occurrence) and
#'  `attr(, "saturation")` (one row per fetched taxon: `taxon_name`,
#'  `n_points`, `n_ecoregions`, `n_batches`, `status` in `c("saturated",
#'  "exhausted", "cap")`).
#'
#' @author Adrien Taudiere
#' @keywords internal
ecoregion_saturation_fetch <- function(
  taxnames = NULL,
  gbif_taxa = NULL,
  n_occur = 1000,
  batch_size = 250,
  min_points = 100,
  patience = 2,
  clean_coord = FALSE,
  time_to_sleep = 0.3,
  verbose = TRUE,
  fetch_page = NULL,
  key_fun = NULL
) {
  if (!is.numeric(n_occur) || length(n_occur) != 1 || n_occur <= 0) {
    cli::cli_abort("{.arg n_occur} must be a positive number")
  }
  if (!is.numeric(batch_size) || length(batch_size) != 1 || batch_size < 1) {
    cli::cli_abort("{.arg batch_size} must be a positive number")
  }
  if (!is.numeric(min_points) || length(min_points) != 1 || min_points < 0) {
    cli::cli_abort("{.arg min_points} must be a non-negative number")
  }
  if (!is.numeric(patience) || length(patience) != 1 || patience < 1) {
    cli::cli_abort("{.arg patience} must be a positive number")
  }

  if (is.null(gbif_taxa)) {
    if (is.null(taxnames) || length(taxnames) == 0) {
      cli::cli_abort(
        "Either {.arg taxnames} or {.arg gbif_taxa} must be provided"
      )
    }
    gbif_taxa <- gbif_match_taxa(unique(taxnames))
  }
  if (nrow(gbif_taxa) == 0) {
    empty <- tibble::tibble(
      taxon_name = character(),
      usageKey = numeric(),
      decimalLongitude = numeric(),
      decimalLatitude = numeric()
    )
    attr(empty, "missing_taxa") <- taxnames
    return(empty)
  }

  if (is.null(fetch_page)) {
    fetch_page <- function(taxon_key, limit, start) {
      gbif_occ_search_page(taxon_key, limit, start, time_to_sleep)
    }
  }
  if (is.null(key_fun)) {
    ecoregions <- load_ecoregions()
    key_fun <- function(batch) {
      points_to_ecoregions(
        longitudes = batch$decimalLongitude,
        latitudes = batch$decimalLatitude,
        ecoregions = ecoregions
      )$ECO_NAME
    }
  }

  if (verbose) {
    pb <- cli::cli_progress_bar(total = nrow(gbif_taxa))
  }

  occ_list <- vector("list", nrow(gbif_taxa))
  sat_rows <- vector("list", nrow(gbif_taxa))

  for (i in seq_len(nrow(gbif_taxa))) {
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
      cli::cli_alert_info(
        "Fetching GBIF occurrences for {.emph {gbif_taxa$verbatim_name[i]}}"
      )
    }

    state <- list(
      n_points = 0,
      seen = character(),
      stale = 0L,
      saturated = FALSE
    )
    batches <- list()
    status <- "cap"
    start <- 0

    repeat {
      limit <- min(batch_size, n_occur - state$n_points)
      if (limit <= 0) {
        status <- "cap"
        break
      }
      batch <- fetch_page(gbif_taxa$usageKey[i], limit, start)
      n_batch <- if (is.null(batch)) 0L else nrow(batch)
      start <- start + n_batch

      if (n_batch > 0) {
        batches[[length(batches) + 1]] <- batch |>
          dplyr::mutate(
            taxon_name = gbif_taxa$verbatim_name[i],
            usageKey = gbif_taxa$usageKey[i],
            .before = 1
          )
        state <- ecoregion_saturation_step(
          state,
          key_fun(batch),
          n_batch,
          min_points,
          patience
        )
      }

      if (state$saturated) {
        status <- "saturated"
        break
      }
      if (n_batch < limit) {
        status <- "exhausted"
        break
      }
      if (state$n_points >= n_occur) {
        status <- "cap"
        break
      }
    }

    occ_list[[i]] <- dplyr::bind_rows(batches)
    sat_rows[[i]] <- tibble::tibble(
      taxon_name = gbif_taxa$verbatim_name[i],
      n_points = state$n_points,
      n_ecoregions = length(state$seen),
      n_batches = length(batches),
      status = status
    )
  }
  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  occ <- dplyr::bind_rows(occ_list)

  if (clean_coord && nrow(occ) > 0) {
    check_package("CoordinateCleaner")
    n_before <- nrow(occ)
    occ <- CoordinateCleaner::clean_coordinates(
      occ,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = "taxon_name",
      verbose = FALSE
    ) |>
      dplyr::filter(.data$.summary)
    if (verbose) {
      cli::cli_alert_info(
        "CoordinateCleaner kept {.val {nrow(occ)}} / {.val {n_before}} rows"
      )
    }
  }

  query_names <- if (is.null(taxnames)) {
    gbif_taxa$verbatim_name
  } else {
    taxnames
  }
  attr(occ, "missing_taxa") <- setdiff(query_names, unique(occ$taxon_name))
  attr(occ, "saturation") <- dplyr::bind_rows(sat_rows)

  occ
}
