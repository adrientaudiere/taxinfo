#' Taxa occurrences check within a radius using GBIF data
#'
#' @description
#' Performs a species occurrence check within a fixed radius around a GPS point using
#'   GBIF occurrence data.
#'
#' @param taxa_name Character. Scientific name of the species to check.
#' @param longitude Numeric. Longitude of the test point in decimal degrees.
#' @param latitude Numeric. Latitude of the test point in decimal degrees.
#' @param radius_km Numeric. Search radius in kilometers (default: 50).
#' @param clean_coord (Logical, default: TRUE). Whether to clean coordinates
#'   using CoordinateCleaner
#' @param circle_form (Logical, default: TRUE). Whether to use a circular
#'   search area. If FALSE, a square bounding box is used.
#' @param info_names Character vector. Columns to select from GBIF data
#' (default:c("decimalLongitude", "decimalLatitude", "country", "year",
#'  "scientificName", "recordedBy", "gbifRegion")). Note that "scientificName",
#' "decimalLongitude" and "decimalLatitude" are required.
#' @param return_all_occ (Logical, default: FALSE). If TRUE, return all occurrences
#' found within the radius in a data frame called "occ_data" in the resulting list.
#' @param verbose (Logical, default: TRUE). Whether to print progress messages.
#' @param clean_coord_verbose (Logical, default: FALSE). Whether to print messages
#'  from CoordinateCleaner.
#' @param n_occur Numeric (default: 1000). Maximum number of occurrences to
#'  retrieve from GBIF.
#' @param ... Additional parameters passed to [rgbif::occ_search()].
#'
#' @return A list containing:
#'  - count_in_radius: Number of occurrences found within the radius
#'  - closest_distance_km: Distance to the closest occurrence in kilometers
#'  - mean_distance_km: Mean distance to all occurrences in kilometers
#'  - total_count_in_world: Total number of occurrences with coordinates worldwide
#'  - search_radius: The search radius used (in kilometers)
#'  - closest_point_lat: Latitude of the closest occurrence
#'  - closest_point_lon: Longitude of the closest occurrence
#'  - sample_point_lat: Latitude of the tested point
#'  - sample_point_lon: Longitude of the tested point
#'  - occ_data (optional, if `return_all_occ` is TRUE): Data frame of all occurrences
#'  found within the radius
#' @seealso [tax_occur_check_pq()], [tax_occur_multi_check_pq()]
#' @examples
#' # Check for Oak species near Paris
#' long <- 2.3522
#' lat <- 48.8566
#'
#' Q_rob_in_Paris <- tax_occur_check("Quercus robur", long, lat, 100)
#' Q_rob_in_Paris
#'
#' tax_occur_check("Trametopsis brasiliensis", long, lat, 100)
#'
#' # Visualize occurrences around Paris for Fagus sylvatica
#' res_occ <- tax_occur_check("Fagus sylvatica", long, lat, 200,
#'   return_all_occ = TRUE
#' )
#'
#' occ_data_sf <- sf::st_as_sf(res_occ$occ_data,
#'   coords = c("decimalLongitude", "decimalLatitude"),
#'   crs = 4326
#' )
#'
#' if (requireNamespace("leaflet")) {
#'   library(leaflet)
#' }
#' if (requireNamespace("leafpop")) {
#'   library(leafpop)
#' }
#' leaflet() |>
#'   addTiles() |>
#'   setView(lat, long, zoom = 12) |>
#'   fitBounds(
#'     lat1 = as.vector(sf::st_bbox(occ_data_sf))[2],
#'     lng1 = as.vector(sf::st_bbox(occ_data_sf))[1],
#'     lat2 = as.vector(sf::st_bbox(occ_data_sf))[4],
#'     lng2 = as.vector(sf::st_bbox(occ_data_sf))[3]
#'   ) |>
#'   leaflet::addCircles(data = occ_data_sf, color = "blue", stroke = 1, opacity = 0.8) |>
#'   leaflet::addCircleMarkers(lat, long, color = "orange", radius = 2, opacity = 1)
#'
#' @author Adrien Taudière
#' @export
tax_occur_check <- function(taxa_name,
                            longitude,
                            latitude,
                            radius_km = 50,
                            circle_form = TRUE,
                            clean_coord = TRUE,
                            info_names = c(
                              "decimalLongitude",
                              "decimalLatitude",
                              "country",
                              "year",
                              "scientificName",
                              "recordedBy",
                              "gbifRegion"
                            ),
                            return_all_occ = FALSE,
                            verbose = TRUE,
                            clean_coord_verbose = FALSE,
                            n_occur = 1000,
                            ...) {
  species_key <- rgbif::name_backbone(taxa_name)$usageKey
  if (is.null(species_key)) {
    stop("Species ", species_key, " not found")
  }

  bbox <- calculate_bbox(
    longitude = longitude,
    latitude = latitude,
    radius_km = radius_km
  )

  occurrences_world_with_coordinate <- rgbif::occ_count(
    taxonKey = species_key,
    hasCoordinate = TRUE
  )
  occurrences <- rgbif::occ_search(
    taxonKey = species_key,
    hasCoordinate = TRUE,
    hasGeospatialIssue = FALSE,
    decimalLongitude = paste(bbox$xmin, bbox$xmax, sep = ","),
    decimalLatitude = paste(bbox$ymin, bbox$ymax, sep = ","),
    limit = n_occur,
    ...
  )

  if (occurrences$meta$count > 0) {
    if (clean_coord) {
      check_package("CoordinateCleaner")
      n_occur_old <- nrow(occurrences$data)
      occurrences$data <-
        CoordinateCleaner::clean_coordinates(occurrences$data,
          lon = "decimalLongitude",
          lat = "decimalLatitude",
          verbose = clean_coord_verbose,
          species = "scientificName"
        ) |>
        filter(.summary)

      if (verbose) {
        remaining_occurrences <- nrow(occurrences$data)
        percentage <- round(100 * remaining_occurrences / n_occur_old, 1)
        cli::cli_alert_info(c(
          "After cleaning with CoordinateCleaner::clean_coordinates:\n",
          "  • {.val {remaining_occurrences}} occurrences remain(s)\n",
          "  • Total original: {.val {n_occur_old}}\n",
          "  • Retention rate: {.val {percentage}}%"
        ))
      }
    }

    occ_data <- occurrences$data

    if (nrow(occ_data) > 0) {
      if (!is.null(info_names)) {
        occ_data <- occ_data |>
          select(any_of(info_names))
      }

      # Calculate actual distances
      test_point <- sf::st_sfc(sf::st_point(c(longitude, latitude)), crs = 4326)
      occ_sf <- sf::st_as_sf(occ_data,
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = 4326
      )

      distances <- sf::st_distance(test_point, occ_sf)
      occ_data$distance_km <- as.numeric(distances) / 1000
      min_distance_km <- as.numeric(min(distances)) / 1000

      if (circle_form) {
        occ_data <- occ_data |>
          filter(distance_km <= radius_km)
      }

      if (verbose) {
        cli::cli_bullets(c(
          "v" = "Found {.val {nrow(occ_data)}} occurrences for species {.emph {taxa_name}}:",
          "*" = "Closest occurrence: {.val {round(min_distance_km, 2)}} km"
        ))
      }
      # Update statistics
      closest_distance_km <- min(min_distance_km)
      mean_distance_km <- mean(as.numeric(distances)) / 1000
    } else {
      if (verbose) {
        cli::cli_alert_warning("No valid occurrences for {.emph {taxa_name}}")
      }
      return(list(
        "count_in_radius" = 0,
        "closest_distance_km" = NA,
        "mean_distance_km" = NA,
        "total_count_in_world" = occurrences_world_with_coordinate,
        "search_radius" = radius_km,
        "closest_point_lat" = NA,
        "closest_point_lon" = NA,
        "sample_point_lat" = latitude,
        "sample_point_lon" = longitude
      ))
    }
  } else {
    if (verbose) {
      cli::cli_alert_warning("No occurrences found for {.emph {taxa_name}}")
    }
    return(list(
      "count_in_radius" = 0,
      "closest_distance_km" = NA,
      "mean_distance_km" = NA,
      "total_count_in_world" = 0,
      "search_radius" = radius_km,
      "closest_point_lat" = NA,
      "closest_point_lon" = NA,
      "sample_point_lat" = latitude,
      "sample_point_lon" = longitude
    ))
  }

  if (return_all_occ) {
    return(list(
      "occ_data" = occ_data,
      "count_in_radius" = nrow(occ_data),
      "closest_distance_km" = ifelse(nrow(occ_data) > 0, round(closest_distance_km, 2), NA),
      "mean_distance_km" = ifelse(nrow(occ_data) > 0, round(mean_distance_km, 2), NA),
      "total_count_in_world" = occurrences_world_with_coordinate,
      "search_radius" = radius_km,
      "closest_point_lat" = occ_data$decimalLatitude[which.min(occ_data$distance_km)],
      "closest_point_lon" = occ_data$decimalLongitude[which.min(occ_data$distance_km)],
      "sample_point_lat" = latitude,
      "sample_point_lon" = longitude
    ))
  } else {
    return(list(
      "count_in_radius" = nrow(occ_data),
      "closest_distance_km" = ifelse(nrow(occ_data) > 0, round(closest_distance_km, 2), NA),
      "mean_distance_km" = ifelse(nrow(occ_data) > 0, round(mean_distance_km, 2), NA),
      "total_count_in_world" = occurrences_world_with_coordinate,
      "search_radius" = radius_km,
      "closest_point_lat" = occ_data$decimalLatitude[which.min(occ_data$distance_km)],
      "closest_point_lon" = occ_data$decimalLongitude[which.min(occ_data$distance_km)],
      "sample_point_lat" = latitude,
      "sample_point_lon" = longitude
    ))
  }
}
