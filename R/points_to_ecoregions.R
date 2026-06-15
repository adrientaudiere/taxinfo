#' Map GPS points to WWF/TNC terrestrial ecoregions
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Assigns each GPS point (pair of longitude/latitude in decimal degrees, WGS84)
#' to the WWF/TNC terrestrial ecoregion, biome and realm that contains it.
#'
#' @param longitudes (numeric vector). Longitudes of the points to locate, in
#'  decimal degrees in `[-180, 180]`.
#' @param latitudes (numeric vector). Latitudes of the points to locate, in
#'  decimal degrees in `[-90, 90]`. Must have the same length as `longitudes`.
#' @param ecoregions (optional `sf` object, default `NULL`). Ecoregion polygon
#'  layer to use. If `NULL`, the shipped WWF/TNC layer is loaded via
#'  [load_ecoregions()] (result is cached, so passing `NULL` is usually the
#'  right choice).
#'
#' @returns A tibble with one row per input point and the columns
#'  `point_id` (integer), `longitude`, `latitude`, `ECO_NAME`, `biome`, `realm`.
#'  Points falling outside any ecoregion (oceans, poles...) have `NA` in the
#'  three ecoregion columns.
#'
#' @author Adrien Taudiere
#' @seealso [tax_check_ecoregion()], [tax_ecoregion_occur()]
#' @examples
#' \dontrun{
#' points_to_ecoregions(
#'   longitudes = c(2.3522, 4.2, -70),
#'   latitudes  = c(48.8566, 33, -33)
#' )
#' }
#' @export
points_to_ecoregions <- function(
  longitudes,
  latitudes,
  ecoregions = NULL
) {
  if (is.null(longitudes) || is.null(latitudes)) {
    cli::cli_abort(
      "Parameters {.arg longitudes} and {.arg latitudes} must be provided"
    )
  }
  if (length(longitudes) != length(latitudes)) {
    cli::cli_abort(
      "{.arg longitudes} and {.arg latitudes} must have the same length"
    )
  }
  if (!is.numeric(longitudes) || !is.numeric(latitudes)) {
    cli::cli_abort(
      "Parameters {.arg longitudes} and {.arg latitudes} must be numeric"
    )
  }
  if (any(longitudes < -180 | longitudes > 180, na.rm = TRUE)) {
    cli::cli_abort("{.arg longitudes} must be in [-180, 180]")
  }
  if (any(latitudes < -90 | latitudes > 90, na.rm = TRUE)) {
    cli::cli_abort("{.arg latitudes} must be in [-90, 90]")
  }

  if (is.null(ecoregions)) {
    ecoregions <- load_ecoregions()
  }

  pts <- sf::st_as_sf(
    tibble::tibble(
      point_id = seq_along(longitudes),
      longitude = longitudes,
      latitude = latitudes
    ),
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  joined <- sf::st_join(pts, ecoregions, join = sf::st_intersects) |>
    sf::st_drop_geometry()

  tibble::as_tibble(joined) |>
    dplyr::transmute(
      point_id = .data$point_id,
      longitude = .data$longitude,
      latitude = .data$latitude,
      ECO_NAME = .data$ECO_NAME,
      biome = .data$WWF_MHTNAM,
      realm = .data$WWF_REALM2
    ) |>
    dplyr::distinct(.data$point_id, .keep_all = TRUE)
}
