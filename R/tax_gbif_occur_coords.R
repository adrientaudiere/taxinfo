#' Get GBIF occurrence coordinates for a vector of taxa
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Retrieves up to `n_occur` georeferenced GBIF occurrences for each name in
#' `taxnames` and returns them as a long tibble. Taxa are resolved to GBIF
#' usage keys once via [rgbif::name_backbone_checklist()] (filtering on
#' `matchType %in% c("EXACT", "HIGHERRANK")`), then occurrences are fetched
#' with [rgbif::occ_search()] (`hasGeospatialIssue = FALSE`). Rows with
#' missing coordinates are dropped.
#'
#' @param taxnames (character vector) Scientific names of the taxa to query.
#' @param n_occur (numeric, default `1000`). Maximum number of occurrences to
#'  retrieve per taxon. Use a smaller value (e.g. `200`) for quick checks.
#' @param clean_coord (logical, default `FALSE`). If `TRUE`, run
#'  [CoordinateCleaner::clean_coordinates()] on the result (requires the
#'  `CoordinateCleaner` package).
#' @param verbose (logical, default `TRUE`). If `TRUE`, print progress messages.
#' @param time_to_sleep (numeric, default `0.3`). Seconds to pause between
#'  [rgbif::occ_search()] calls to avoid GBIF rate-limiting.
#'
#' @returns A tibble with columns `taxon_name`, `usageKey`, `decimalLongitude`,
#'  `decimalLatitude`, `countryCode`, `year`, `gbifID`. Taxa with zero valid
#'  occurrences are listed in `attr(result, "missing_taxa")`.
#'
#' @author Adrien Taudiere
#' @seealso [tax_ecoregion_occur()], [rgbif::occ_search()]
#' @examples
#' \dontrun{
#' tax_gbif_occur_coords(
#'   c("Xylobolus subpileatus", "Amanita muscaria"),
#'   n_occur = 200
#' )
#' }
#' @export
tax_gbif_occur_coords <- function(
  taxnames,
  n_occur = 1000,
  clean_coord = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3
) {
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

  if (nrow(gbif_taxa) == 0) {
    cli::cli_alert_warning("No taxa matched in the GBIF backbone")
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
    return(res)
  }

  keep_cols <- c(
    "decimalLongitude",
    "decimalLatitude",
    "countryCode",
    "year",
    "gbifID",
    "scientificName"
  )

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

  tib_occur <- bind_rows(occ_list)

  if (nrow(tib_occur) == 0) {
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
    return(res)
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
