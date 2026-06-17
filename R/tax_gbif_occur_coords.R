#' Get GBIF occurrence coordinates for a vector of taxa
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Retrieves georeferenced GBIF occurrences for each name in `taxnames` and
#' returns them as a long tibble. Taxa are resolved to GBIF usage keys once via
#' [rgbif::name_backbone_checklist()] (filtering on
#' `matchType %in% c("EXACT", "HIGHERRANK")`), then occurrences are fetched
#' with one of three methods (see `method`). Rows with missing coordinates are
#' dropped.
#'
#' @param taxnames (character vector) Scientific names of the taxa to query.
#' @param n_occur (numeric, default `1000`). Maximum number of occurrences to
#'  keep per taxon. With `method = "search"` this is a server-side limit; with
#'  the download methods it is applied as a local sample after import (a warning
#'  is issued when a taxon exceeded `n_occur`).
#' @param method (character, default `"download"`). How occurrences are fetched:
#'  - `"download"`: a single [rgbif::occ_download()] request for all taxa at
#'    once (no 100,000-record cap, mints a citable DOI). **Requires GBIF
#'    credentials** (see [check_gbif_credentials()]).
#'  - `"download_sql"`: [rgbif::occ_download_sql()] with server-side column
#'    selection and `WHERE` filtering (gated preview, must be enabled for your
#'    account). **Requires GBIF credentials.** Because GBIF SQL `taxonkey` is not
#'    hierarchical, this method matches `taxonkey`/`specieskey` directly and may
#'    under-return records for names matched at a higher rank (`HIGHERRANK`); use
#'    `"download"` if you need full hierarchical coverage.
#'  - `"search"`: the legacy per-taxon [rgbif::occ_search()] loop (fast, capped
#'    at 100,000 records, no credentials).
#' @param country (character, default `NULL`). Optional ISO2 country code used as
#'  a server-side filter for the download methods (e.g. `"FR"`).
#' @param year_gte,year_lte (numeric, default `NULL`). Optional inclusive year
#'  bounds used as server-side filters for the download methods.
#' @param geometry (character, default `NULL`). Optional WKT polygon used as a
#'  server-side spatial filter for `method = "download"`
#'  (via [rgbif::pred_within()]). Not supported with `method = "download_sql"`.
#' @param clean_coord (logical, default `FALSE`). If `TRUE`, run
#'  [CoordinateCleaner::clean_coordinates()] on the result (requires the
#'  `CoordinateCleaner` package).
#' @param verbose (logical, default `TRUE`). If `TRUE`, print progress messages.
#' @param time_to_sleep (numeric, default `0.3`). Seconds to pause between
#'  [rgbif::occ_search()] calls to avoid GBIF rate-limiting. Only used when
#'  `method = "search"`.
#'
#' @returns A tibble with columns `taxon_name`, `usageKey`, `decimalLongitude`,
#'  `decimalLatitude`, `countryCode`, `year`, `gbifID`. Taxa with zero valid
#'  occurrences are listed in `attr(result, "missing_taxa")`.
#'
#' @author Adrien Taudiere
#' @seealso [tax_ecoregion_occur()], [rgbif::occ_download()],
#'  [rgbif::occ_download_sql()], [rgbif::occ_search()]
#' @examples
#' \dontrun{
#' # Default: GBIF Download API (requires GBIF_USER, GBIF_PWD, GBIF_EMAIL)
#' tax_gbif_occur_coords(
#'   c("Xylobolus subpileatus", "Amanita muscaria"),
#'   n_occur = 200
#' )
#'
#' # Narrow the download server-side to reduce transfer
#' tax_gbif_occur_coords(
#'   c("Amanita muscaria"),
#'   country = "FR",
#'   year_gte = 2000
#' )
#'
#' # Legacy fast path (no credentials, capped at 100,000 records)
#' tax_gbif_occur_coords(
#'   c("Xylobolus subpileatus"),
#'   method = "search",
#'   n_occur = 200
#' )
#' }
#' @export
tax_gbif_occur_coords <- function(
  taxnames,
  n_occur = 1000,
  method = c("download", "download_sql", "search"),
  country = NULL,
  year_gte = NULL,
  year_lte = NULL,
  geometry = NULL,
  clean_coord = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3
) {
  method <- match.arg(method)

  if (is.null(taxnames) || length(taxnames) == 0) {
    cli::cli_abort("{.arg taxnames} must be a non-empty character vector")
  }
  if (!is.character(taxnames)) {
    cli::cli_abort("{.arg taxnames} must be a character vector")
  }
  if (any(!nzchar(taxnames))) {
    cli::cli_abort("{.arg taxnames} must not contain empty strings")
  }
  if (!is.numeric(n_occur) || length(n_occur) != 1 || n_occur <= 0) {
    cli::cli_abort("{.arg n_occur} must be a positive number")
  }

  taxnames <- unique(taxnames)

  gbif_taxa <- rgbif::name_backbone_checklist(taxnames) |>
    filter(.data$matchType %in% c("EXACT", "HIGHERRANK")) |>
    distinct(.data$usageKey, .data$canonicalName, .data$verbatim_name)

  empty_result <- function() {
    res <- tibble::tibble(
      taxon_name = character(),
      usageKey = integer(),
      decimalLongitude = numeric(),
      decimalLatitude = numeric(),
      countryCode = character(),
      year = integer(),
      gbifID = character()
    )
    attr(res, "missing_taxa") <- taxnames
    res
  }

  if (nrow(gbif_taxa) == 0) {
    cli::cli_alert_warning("No taxa matched in the GBIF backbone")
    return(empty_result())
  }

  keep_cols <- c(
    "decimalLongitude",
    "decimalLatitude",
    "countryCode",
    "year",
    "gbifID",
    "scientificName"
  )

  if (method == "search") {
    tib_occur <- gbif_occur_coords_search(
      gbif_taxa = gbif_taxa,
      n_occur = n_occur,
      keep_cols = keep_cols,
      verbose = verbose,
      time_to_sleep = time_to_sleep
    )
  } else {
    tib_occur <- gbif_occur_coords_download(
      gbif_taxa = gbif_taxa,
      n_occur = n_occur,
      keep_cols = keep_cols,
      method = method,
      country = country,
      year_gte = year_gte,
      year_lte = year_lte,
      geometry = geometry,
      verbose = verbose
    )
  }

  if (is.null(tib_occur) || nrow(tib_occur) == 0) {
    return(empty_result())
  }

  tib_occur <- tib_occur |>
    filter(
      !is.na(.data$decimalLongitude),
      !is.na(.data$decimalLatitude)
    )

  if (clean_coord) {
    check_package("CoordinateCleaner")
    n_before <- nrow(tib_occur)
    tib_occur <- CoordinateCleaner::clean_coordinates(
      tib_occur,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = "taxon_name",
      verbose = FALSE
    ) |>
      filter(.data$.summary)
    if (verbose) {
      cli::cli_alert_info(
        "CoordinateCleaner kept {.val {nrow(tib_occur)}} / {.val {n_before}} rows"
      )
    }
  }

  tib_occur <- tib_occur |>
    select(
      "taxon_name",
      "usageKey",
      "decimalLongitude",
      "decimalLatitude",
      any_of(c("countryCode", "year", "gbifID"))
    )

  missing <- setdiff(taxnames, unique(tib_occur$taxon_name))
  if (length(missing) > 0 && verbose) {
    cli::cli_alert_warning(
      "No GBIF occurrences retrieved for: {.emph {missing}}"
    )
  }
  attr(tib_occur, "missing_taxa") <- missing

  tib_occur
}


#' Per-taxon `occ_search` loop for [tax_gbif_occur_coords()]
#'
#' @inheritParams tax_gbif_occur_coords
#' @param gbif_taxa (tibble) Resolved GBIF taxa with `usageKey`, `canonicalName`
#'  and `verbatim_name`.
#' @param keep_cols (character) Occurrence columns to retain.
#' @returns A tibble of occurrences with `taxon_name` and `usageKey`, or `NULL`.
#' @author Adrien Taudiere
#' @keywords internal
gbif_occur_coords_search <- function(
  gbif_taxa,
  n_occur,
  keep_cols,
  verbose = TRUE,
  time_to_sleep = 0.3
) {
  if (verbose) {
    pb <- cli::cli_progress_bar(total = nrow(gbif_taxa))
  }

  occ_list <- vector("list", nrow(gbif_taxa))
  for (i in seq_len(nrow(gbif_taxa))) {
    Sys.sleep(time_to_sleep)
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
      cli::cli_alert_info(
        "Fetching GBIF occurrences for {.emph {gbif_taxa$verbatim_name[i]}}"
      )
    }
    res_i <- rgbif::occ_search(
      taxonKey = gbif_taxa$usageKey[i],
      limit = n_occur,
      hasCoordinate = TRUE,
      hasGeospatialIssue = FALSE
    )$data

    if (!is.null(res_i) && nrow(res_i) > 0) {
      res_i <- res_i |>
        select(any_of(keep_cols)) |>
        mutate(
          taxon_name = gbif_taxa$verbatim_name[i],
          usageKey = gbif_taxa$usageKey[i]
        )
      occ_list[[i]] <- res_i
    }
  }
  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  bind_rows(occ_list)
}


#' Single GBIF download for [tax_gbif_occur_coords()]
#'
#' @inheritParams tax_gbif_occur_coords
#' @param gbif_taxa (tibble) Resolved GBIF taxa with `usageKey`, `canonicalName`
#'  and `verbatim_name`.
#' @param keep_cols (character) Occurrence columns to retain.
#' @returns A tibble of occurrences with `taxon_name` and `usageKey`, or `NULL`.
#' @author Adrien Taudiere
#' @keywords internal
gbif_occur_coords_download <- function(
  gbif_taxa,
  n_occur,
  keep_cols,
  method = "download",
  country = NULL,
  year_gte = NULL,
  year_lte = NULL,
  geometry = NULL,
  verbose = TRUE
) {
  no_filter <- is.null(country) &&
    is.null(year_gte) &&
    is.null(year_lte) &&
    is.null(geometry)
  if (verbose && no_filter) {
    cli::cli_alert_info(c(
      "GBIF downloads retrieve {.strong all} matching records. ",
      "Consider narrowing with {.arg country}, {.arg year_gte}, ",
      "{.arg year_lte} or {.arg geometry} to reduce the transfer."
    ))
  }

  keys <- gbif_taxa$usageKey

  if (method == "download_sql") {
    if (!is.null(geometry)) {
      cli::cli_abort(
        "{.arg geometry} is not supported with {.code method = \"download_sql\"}; use {.code method = \"download\"}."
      )
    }
    sql <- build_gbif_coords_sql(
      keys = keys,
      country = country,
      year_gte = year_gte,
      year_lte = year_lte
    )
    occ_data <- gbif_download(sql = sql, verbose = verbose)
  } else {
    preds <- list(
      rgbif::pred_in("taxonKey", keys),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred("hasGeospatialIssue", FALSE)
    )
    if (!is.null(country)) {
      preds <- c(preds, list(rgbif::pred("country", country)))
    }
    if (!is.null(year_gte)) {
      preds <- c(preds, list(rgbif::pred_gte("year", year_gte)))
    }
    if (!is.null(year_lte)) {
      preds <- c(preds, list(rgbif::pred_lte("year", year_lte)))
    }
    if (!is.null(geometry)) {
      preds <- c(preds, list(rgbif::pred_within(geometry)))
    }
    occ_data <- do.call(gbif_download, c(preds, list(verbose = verbose)))
  }

  if (is.null(occ_data) || nrow(occ_data) == 0) {
    return(NULL)
  }

  # Normalise column names (SIMPLE_CSV is camelCase, SQL output is lowercase).
  canonical <- c(
    "taxonKey",
    "speciesKey",
    "species",
    "genus",
    "family",
    "order",
    "class",
    "phylum",
    "kingdom",
    keep_cols
  )
  idx <- match(tolower(names(occ_data)), tolower(canonical))
  names(occ_data)[!is.na(idx)] <- canonical[idx[!is.na(idx)]]

  # Attribute each record to the queried taxon (hierarchical download returns
  # descendants whose own taxonKey differs from the queried key).
  occ_data <- attribute_gbif_records(occ_data, gbif_taxa)
  if (is.null(occ_data) || nrow(occ_data) == 0) {
    return(NULL)
  }

  # Apply the per-taxon cap locally (GBIF downloads have no server-side limit).
  occ_list <- vector("list", nrow(gbif_taxa))
  for (i in seq_len(nrow(gbif_taxa))) {
    d_i <- occ_data |> filter(.data$usageKey == gbif_taxa$usageKey[i])
    if (nrow(d_i) > n_occur) {
      if (verbose) {
        cli::cli_alert_warning(
          "{.emph {gbif_taxa$verbatim_name[i]}}: {.val {nrow(d_i)}} records downloaded, sampled to {.val {n_occur}}"
        )
      }
      d_i <- d_i |> slice_sample(n = n_occur)
    }
    occ_list[[i]] <- d_i
  }

  bind_rows(occ_list)
}


#' Build the SQL query used by [tax_gbif_occur_coords()] download_sql method
#'
#' @param keys (integer) GBIF usage keys.
#' @param country (character or NULL) ISO2 country code.
#' @param year_gte,year_lte (numeric or NULL) Inclusive year bounds.
#' @returns A single SQL query string for [rgbif::occ_download_sql()].
#' @author Adrien Taudiere
#' @keywords internal
build_gbif_coords_sql <- function(
  keys,
  country = NULL,
  year_gte = NULL,
  year_lte = NULL
) {
  key_list <- paste(keys, collapse = ", ")
  # GBIF SQL columns are literal record values: `taxonkey` is NOT hierarchical
  # (unlike the predicate API). Match `specieskey` too so a species-level query
  # still retrieves its infraspecific records. `species`/`genus` are selected so
  # that [attribute_gbif_records()] can fall back to name matching.
  where <- c(
    paste0("(taxonkey IN (", key_list, ") OR specieskey IN (", key_list, "))"),
    "hascoordinate = TRUE",
    "hasgeospatialissue = FALSE"
  )
  if (!is.null(country)) {
    where <- c(where, paste0("countrycode = '", country, "'"))
  }
  if (!is.null(year_gte)) {
    where <- c(where, paste0("year >= ", year_gte))
  }
  if (!is.null(year_lte)) {
    where <- c(where, paste0("year <= ", year_lte))
  }

  paste0(
    "SELECT taxonkey, specieskey, species, genus, decimallongitude, ",
    "decimallatitude, countrycode, year, gbifid, scientificname ",
    "FROM occurrence WHERE ",
    paste(where, collapse = " AND ")
  )
}
