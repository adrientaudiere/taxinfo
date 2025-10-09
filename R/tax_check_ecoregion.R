#' Check if a GPS point is within an ecoregion where the species is present
#'
#' @description
#' This function determines whether a given GPS point falls within an ecoregion
#' where a species has been observed, using GBIF occurrence data
#' and WWF ecoregion data.
#'
#' @param taxa_name (character) Scientific name of the species to check.
#' @param longitudes (numeric vector) Longitude of the points to test
#' @param latitudes (numeric vector) Latitude of the points to test
#' @param n_occur numeric (default: 500) Maximum number of occurrences to
#'  retrieve from GBIF
#' @param min_proportion numeric (default: 0) Minimum proportion of occurrences
#' in an ecoregion for it to be considered part of the species range (0 to 1).
#' Note that min_proportion and min_nb_occur are combined (AND operator) when
#' both are used.
#' @param min_nb_occur numeric (default: 0) Minimum number of occurrences
#' in an ecoregion for it to be considered part of the species range. Note that
#' min_proportion and min_nb_occur are combined (AND operator) when both are
#' used.
#' @param verbose logical (default: TRUE) Whether to print progress messages
#'
#' @return  A list containing:
#' - ecoregion: A named vector with the number of occurrences in each
#' ecoregion for the species
#' - points_ecoregion: A vector with the ecoregion of each tested
#' GPS point
#' - is_in_ecoregion: TRUE if at least `min_nb_occur` of the tested GPS points falls within
#' an ecoregion where the species has occurrences, FALSE otherwise
#' @author Adrien Taudière
#' @details
#' The function:
#' 1. Extracts ecoregions from species occurrences
#' 2. Determines the ecoregion of the tested GPS point
#' 3. Checks if this ecoregion matches those of the species
#'
#' @seealso [tax_occur_check()], [tax_occur_multi_check_pq()], [tax_occur_check_pq()]
#' @examples
#' \dontrun{
#' # Get occurrences
#' requireNamespace(rgbif)
#' tax_check_ecoregion("Xylobolus subpileatus",
#'   longitudes = c(2.3522, 4.2),
#'   latitudes = c(48.8566, 33)
#' )
#'
#' xylo_ecoregion <- tax_check_ecoregion("Xylobolus subpileatus",
#'   longitudes = c(2.3522, 4.2), latitudes = c(48.8566, 33),
#'   n_occur = 20
#' )
#' }
#'
#' @export
tax_check_ecoregion <- function(taxa_name,
                                longitudes = NULL,
                                latitudes = NULL,
                                n_occur = 500,
                                min_proportion = 0,
                                min_nb_occur = 0,
                                verbose = TRUE) {
  if (length(longitudes) != length(latitudes)) {
    cli::cli_abort("Parameters {.arg longitudes} and {.arg latitudes} must have the same length")
  }
  if (verbose) {
    cli::cli_alert_info("Downloading ecoregion data for {.emph {taxa_name}}")
  }
  occurrences <- rgbif::occ_search(
    scientificName = taxa_name,
    limit = n_occur,
    hasGeospatialIssue = FALSE
  )$data

  clean_occurrences <- occurrences |>
    filter(!is.na(decimalLongitude), !is.na(decimalLatitude))

  if (nrow(clean_occurrences) == 0) {
    cli::cli_alert_warning("No valid occurrences found")
    return(FALSE)
  }

  if (verbose) {
    cli::cli_alert_info("Downloading and validating ecoregion data")
  }
  gbif.range::check_and_get_bioreg("eco_terra")

  ecoregions <- gbif.range::read_bioreg(
    bioreg_name = "eco_terra",
    save_dir = NULL
  ) |>
    sf::st_as_sf() |>
    sf::st_make_valid()

  occurrences_sf <- sf::st_as_sf(clean_occurrences,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326
  )

  samples_point <- sf::st_as_sf(data.frame(lon = longitudes, lat = latitudes),
    coords = c("lon", "lat"), crs = 4326
  )
  if (verbose) {
    cli::cli_alert_info("Listing ecoregions for {.emph {taxa_name}}")
  }

  species_ecoregions <- sf::st_intersection(occurrences_sf, ecoregions) |>
    sf::st_drop_geometry()

  if (nrow(species_ecoregions) == 0) {
    cli::cli_alert_warning("No ecoregions found for species occurrences")
    return(FALSE)
  }

  ecoregions_list <- table(species_ecoregions$ECO_NAME) |>
    sort(decreasing = TRUE) |>
    (\(tab) tab[as.numeric(tab) > min_proportion * nrow(species_ecoregions)])()
  if (verbose) {
    cli::cli_alert_info("Listing ecoregions for {.val {length(longitudes)}} GPS points")
  }

  points_ecoregion <- sf::st_intersection(samples_point, ecoregions) |>
    sf::st_drop_geometry()

  return(list(
    "ecoregion" = ecoregions_list,
    "points_ecoregion" = points_ecoregion,
    "is_in_ecoregion" = sum(points_ecoregion$ECO_NAME %in% ecoregions_list) > min_nb_occur
  ))
}
