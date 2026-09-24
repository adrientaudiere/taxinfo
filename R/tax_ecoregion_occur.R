#' Count GBIF occurrences of taxa in each WWF/TNC terrestrial ecoregion
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' For each name in `taxnames`, retrieves GBIF occurrence coordinates
#' ([tax_gbif_occur_coords()]), maps them to WWF/TNC terrestrial ecoregions in
#' a single spatial join and returns a long tibble with the number and the
#' proportion of occurrences per (taxon, ecoregion). Use
#' [tax_ecoregion_occur_pq()] for the phyloseq wrapper, and
#' [tax_check_ecoregion()] to compare the profile to specific GPS points.
#'
#' @inheritParams tax_gbif_occur_coords
#' @param method (character, default `"search"`). How GBIF occurrences are
#'  fetched, passed to [tax_gbif_occur_coords()]. Ecoregion profiling defaults to
#'  the credential-free, per-taxon-capped `"search"` path; set `"download"` (or
#'  `"download_sql"`) to use the Download API (**requires GBIF credentials**).
#' @param min_nb_occur (numeric, default `0`). Keep only (taxon, ecoregion)
#'  pairs with at least this many occurrences.
#' @param min_proportion (numeric, default `0`). Keep only (taxon, ecoregion)
#'  pairs whose share of the taxon's total occurrences is `>= min_proportion`
#'  (a number in `[0, 1]`). Combined with `min_nb_occur` via AND.
#' @param saturation (logical, default `FALSE`). If `TRUE` (and
#'  `method = "search"`), occurrences are fetched in batches of `batch_size`
#'  and fetching stops as soon as the taxon's ecoregion set has stopped
#'  growing for `patience` consecutive batches (and at least `min_points`
#'  points were fetched). This directly minimises the number of GBIF points
#'  needed per taxon: once the ecoregion set is stable, the remaining GBIF
#'  records cannot change the profile and are not downloaded. Use
#'  `saturation = FALSE` to always fetch the full `n_occur` budget. Ignored
#'  with the download methods (a GBIF download is a single one-shot request).
#'  The per-taxon fetch summary is the `"saturation"` attribute of the long
#'  tibble built by [tax_ecoregion_occur()]: `attr(result, "saturation")`
#'  for [tax_ecoregion_occur()] and for [tax_ecoregion_occur_pq()] with
#'  `add_to_phyloseq = FALSE`, and `attr(result$taxon_ecoregions,
#'  "saturation")` for [tax_check_ecoregion()]. It is not returned when
#'  [tax_ecoregion_occur_pq()] outputs a phyloseq object
#'  (`add_to_phyloseq = TRUE`).
#' @param batch_size (numeric, default `250`). Page size of each GBIF fetch
#'  when `saturation = TRUE`.
#' @param min_points (numeric, default `100`). Minimum number of points
#'  fetched per taxon before the saturation rule is allowed to fire.
#' @param patience (numeric, default `2`). Number of consecutive batches
#'  without a new ecoregion tolerated before fetching stops.
#'
#' @returns A tibble with columns `taxon_name`, `ECO_NAME`, `biome`, `realm`,
#'  `n_occur`, `prop_occur`. Taxa with zero retrievable occurrences appear
#'  once with `NA` in the ecoregion columns and `n_occur = 0L`, so downstream
#'  joins do not silently drop them. When the saturation early-stop ran,
#'  `attr(result, "saturation")` holds one row per fetched taxon
#'  (`taxon_name`, `n_points`, `n_ecoregions`, `n_batches`, `status`).
#'
#' @author Adrien Taudiere
#' @seealso [tax_gbif_occur_coords()], [tax_check_ecoregion()],
#'  [tax_ecoregion_occur_pq()]
#' @examples
#' \dontrun{
#' tax_ecoregion_occur(
#'   c("Xylobolus subpileatus", "Amanita muscaria"),
#'   n_occur = 200
#' )
#' }
#' @export
tax_ecoregion_occur <- function(
  taxnames,
  n_occur = 1000,
  method = "search",
  min_nb_occur = 0,
  min_proportion = 0,
  clean_coord = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3,
  saturation = FALSE,
  batch_size = 250,
  min_points = 100,
  patience = 2
) {
  if (!is.numeric(min_nb_occur) || min_nb_occur < 0) {
    cli::cli_abort("{.arg min_nb_occur} must be a non-negative number")
  }
  if (
    !is.numeric(min_proportion) ||
      min_proportion < 0 ||
      min_proportion > 1
  ) {
    cli::cli_abort("{.arg min_proportion} must be in [0, 1]")
  }

  if (saturation && method == "search") {
    occ <- ecoregion_saturation_fetch(
      taxnames = taxnames,
      n_occur = n_occur,
      batch_size = batch_size,
      min_points = min_points,
      patience = patience,
      clean_coord = clean_coord,
      time_to_sleep = time_to_sleep,
      verbose = verbose
    )
  } else {
    if (saturation && verbose) {
      cli::cli_alert_info(
        "Saturation early-stop only applies to {.code method = \"search\"}; fetching up to {.val {n_occur}} records with {.code method = {.val {method}}}."
      )
    }
    occ <- tax_gbif_occur_coords(
      taxnames = taxnames,
      n_occur = n_occur,
      method = method,
      clean_coord = clean_coord,
      verbose = verbose,
      time_to_sleep = time_to_sleep
    )
  }
  missing_taxa <- attr(occ, "missing_taxa")
  saturation_info <- attr(occ, "saturation")

  empty_row <- function(tn) {
    tibble::tibble(
      taxon_name = tn,
      ECO_NAME = NA_character_,
      biome = NA_character_,
      realm = NA_character_,
      n_occur = 0L,
      prop_occur = NA_real_
    )
  }

  if (nrow(occ) == 0) {
    res <- dplyr::bind_rows(lapply(unique(taxnames), empty_row))
    if (!is.null(saturation_info)) {
      attr(res, "saturation") <- saturation_info
    }
    return(res)
  }

  ecoregions <- load_ecoregions()

  occ_sf <- sf::st_as_sf(
    occ,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326
  )

  joined <- sf::st_join(occ_sf, ecoregions, join = sf::st_intersects) |>
    sf::st_drop_geometry()

  total_per_taxon <- joined |>
    dplyr::group_by(.data$taxon_name) |>
    dplyr::summarise(total = dplyr::n(), .groups = "drop")

  summary_tbl <- joined |>
    dplyr::filter(!is.na(.data$ECO_NAME)) |>
    dplyr::count(
      .data$taxon_name,
      .data$ECO_NAME,
      .data$WWF_MHTNAM,
      .data$WWF_REALM2,
      name = "n_occur"
    ) |>
    dplyr::rename(biome = "WWF_MHTNAM", realm = "WWF_REALM2") |>
    dplyr::left_join(total_per_taxon, by = "taxon_name") |>
    dplyr::mutate(prop_occur = .data$n_occur / .data$total) |>
    dplyr::select(-"total") |>
    dplyr::filter(
      .data$n_occur >= min_nb_occur,
      .data$prop_occur >= min_proportion
    ) |>
    dplyr::arrange(.data$taxon_name, dplyr::desc(.data$n_occur))

  covered <- unique(summary_tbl$taxon_name)
  not_covered <- setdiff(unique(occ$taxon_name), covered)
  if (length(not_covered) > 0) {
    summary_tbl <- dplyr::bind_rows(
      summary_tbl,
      dplyr::bind_rows(lapply(not_covered, empty_row))
    )
  }

  if (length(missing_taxa) > 0) {
    summary_tbl <- dplyr::bind_rows(
      summary_tbl,
      dplyr::bind_rows(lapply(missing_taxa, empty_row))
    )
  }

  res <- tibble::as_tibble(summary_tbl)
  if (!is.null(saturation_info)) {
    attr(res, "saturation") <- saturation_info
  }
  res
}
