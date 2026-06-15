#' Check whether GPS points fall in ecoregions occupied by a set of taxa
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' For each name in `taxnames` (or for each taxon of a `physeq` object), checks
#' whether a set of test GPS points lie within a WWF/TNC terrestrial ecoregion
#' that is present in the taxon's GBIF range. The function is a thin
#' comparison wrapper around [tax_ecoregion_occur()] (for the taxa) and
#' [points_to_ecoregions()] (for the test points).
#'
#' @inheritParams tax_ecoregion_occur_pq
#' @param longitudes (numeric vector) Longitudes of the points to test.
#' @param latitudes (numeric vector) Latitudes of the points to test. Must
#'  have the same length as `longitudes`.
#'
#' @returns A list with four elements:
#' - `taxon_ecoregions`: the long tibble produced by [tax_ecoregion_occur()].
#' - `points_ecoregion`: the tibble produced by [points_to_ecoregions()].
#' - `is_in_ecoregion`: a logical matrix with rownames = taxon names and
#'   colnames = `"point_<i>"`, shape `n_taxa x n_points`. `TRUE` means the
#'   ecoregion of the point is among the taxon's ecoregions that pass
#'   `min_nb_occur` / `min_proportion`.
#' - `ecoregion`: a named list (one named integer vector per taxon) kept for
#'   backward compatibility with earlier versions; prefer `taxon_ecoregions`.
#'
#' @details
#' The previous positional signature `tax_check_ecoregion(taxa_name, lon, lat)`
#' is no longer supported: the first argument is now `physeq`. Use
#' `tax_check_ecoregion(taxnames = "Sp.", longitudes = lon, latitudes = lat)`
#' for single-species calls.
#'
#' @author Adrien Taudiere
#' @seealso [tax_ecoregion_occur()], [tax_ecoregion_occur_pq()],
#'  [points_to_ecoregions()], [tax_occur_check()]
#' @examples
#' \dontrun{
#' requireNamespace("rgbif")
#' res <- tax_check_ecoregion(
#'   taxnames = "Xylobolus subpileatus",
#'   longitudes = c(2.3522, 4.2),
#'   latitudes  = c(48.8566, 33),
#'   n_occur = 200
#' )
#' res$is_in_ecoregion
#' }
#' @export
tax_check_ecoregion <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  longitudes,
  latitudes,
  n_occur = 1000,
  min_nb_occur = 0,
  min_proportion = 0,
  clean_coord = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE
) {
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort(
      "You must specify either {.arg physeq} or {.arg taxnames}, not both"
    )
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }
  if (missing(longitudes) || missing(latitudes)) {
    cli::cli_abort(
      "{.arg longitudes} and {.arg latitudes} must be provided"
    )
  }

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = discard_genus_alone,
      discard_NA = discard_NA
    )
  }

  ecoregions <- load_ecoregions()

  taxon_tbl <- tax_ecoregion_occur(
    taxnames = taxnames,
    n_occur = n_occur,
    min_nb_occur = min_nb_occur,
    min_proportion = min_proportion,
    clean_coord = clean_coord,
    verbose = verbose,
    time_to_sleep = time_to_sleep
  )

  points_tbl <- points_to_ecoregions(
    longitudes = longitudes,
    latitudes = latitudes,
    ecoregions = ecoregions
  )

  is_in <- matrix(
    FALSE,
    nrow = length(unique(taxnames)),
    ncol = length(longitudes),
    dimnames = list(
      unique(taxnames),
      paste0("point_", seq_along(longitudes))
    )
  )
  for (tn in rownames(is_in)) {
    taxon_eco <- taxon_tbl |>
      dplyr::filter(
        .data$taxon_name == tn,
        !is.na(.data$ECO_NAME)
      ) |>
      dplyr::pull(.data$ECO_NAME)
    is_in[tn, ] <- points_tbl$ECO_NAME %in% taxon_eco
  }

  ecoregion_list <- lapply(
    split(taxon_tbl, taxon_tbl$taxon_name),
    function(df) {
      df <- df[!is.na(df$ECO_NAME), , drop = FALSE]
      stats::setNames(as.integer(df$n_occur), df$ECO_NAME)
    }
  )

  list(
    taxon_ecoregions = taxon_tbl,
    points_ecoregion = points_tbl,
    is_in_ecoregion = is_in,
    ecoregion = ecoregion_list
  )
}
