#' Taxa occurrences check within a radius using GBIF data
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Performs a species occurrence check within a fixed radius around a GPS point using
#'   GBIF occurrence data.
#'
#' @param taxa_name Character. Scientific name of the species to check.
#' @param longitude Numeric. Longitude of the test point in decimal degrees.
#' @param latitude Numeric. Latitude of the test point in decimal degrees.
#' @param radius_km Numeric. Search radius in kilometers (default: 50).
#' @param method (character, default `"download"`). How occurrences are fetched:
#'   - `"download"`: a single [rgbif::occ_download()] request constrained to the
#'     search bounding box (mints a citable DOI). **Requires GBIF credentials**
#'     (see [check_gbif_credentials()]).
#'   - `"search"`: the legacy [rgbif::occ_search()] call (fast, capped at
#'     `n_occur` records, no credentials).
#' @param clean_coord (Logical, default: TRUE). Whether to clean coordinates
#'   using CoordinateCleaner
#' @param circle_form (Logical, default: TRUE). Whether to use a circular
#'   search area. If FALSE, a square bounding box is used.
#' @param info_names Character vector. Columns to select from GBIF data
#' (default:c("decimalLongitude", "decimalLatitude", "country", "year",
#'  "scientificName", "recordedBy", "gbifRegion")). Note that "scientificName",
#' "decimalLongitude" and "decimalLatitude" are required. With
#' `method = "download"`, `"country"` is mapped to `"countryCode"` and
#' download-only absent columns (e.g. `"gbifRegion"`) are silently dropped.
#' @param return_all_occ (Logical, default: FALSE). If TRUE, return all occurrences
#' found within the radius in a data frame called "occ_data" in the resulting list.
#' @param verbose (Logical, default: TRUE). Whether to print progress messages.
#' @param clean_coord_verbose (Logical, default: FALSE). Whether to print messages
#'  from CoordinateCleaner.
#' @param n_occur Numeric (default: 1000). Maximum number of occurrences to
#'  retrieve from GBIF. A server-side limit with `method = "search"`; applied as
#'  a local sample after import with `method = "download"`.
#' @param ... Additional parameters passed to [rgbif::occ_search()] (only used
#'  when `method = "search"`).
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
#' @seealso [tax_occur_check_pq()], [tax_occur_multi_check_pq()],
#'  [rgbif::occ_download()]
#' @examples
#' \dontrun{
#' # Check for Oak species near Paris
#' long <- 2.3522
#' lat <- 48.8566
#'
#' Q_rob_in_Paris <- tax_occur_check("Quercus robur", long, lat, radius_km=10)
#' Q_rob_in_Paris
#'
#' tax_occur_check("Trametopsis brasiliensis", long, lat, radius_km=100)
#'
#'
#' # Visualize occurrences around Paris for Fagus sylvatica
#' res_occ <- tax_occur_check("Fagus sylvatica", long, lat, radius_km=20,
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
#' }
#'
#' @author Adrien Taudiere
#' @export
tax_occur_check <- function(
  taxa_name,
  longitude,
  latitude,
  radius_km = 50,
  method = c("download", "search"),
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
  ...
) {
  method <- match.arg(method)

  if (
    is.null(taxa_name) ||
      !is.character(taxa_name) ||
      length(taxa_name) != 1 ||
      !nzchar(taxa_name)
  ) {
    cli::cli_abort("{.arg taxa_name} must be a non-empty character string")
  }
  if (
    !is.numeric(longitude) ||
      length(longitude) != 1 ||
      longitude < -180 ||
      longitude > 180
  ) {
    cli::cli_abort(
      "{.arg longitude} must be a single number between -180 and 180"
    )
  }
  if (
    !is.numeric(latitude) ||
      length(latitude) != 1 ||
      latitude < -90 ||
      latitude > 90
  ) {
    cli::cli_abort("{.arg latitude} must be a single number between -90 and 90")
  }
  if (!is.numeric(radius_km) || length(radius_km) != 1 || radius_km <= 0) {
    cli::cli_abort("{.arg radius_km} must be a single positive number")
  }

  species_key <- rgbif::name_backbone(taxa_name)$usageKey
  if (is.null(species_key)) {
    stop("Species ", taxa_name, " not found")
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

  empty_result <- function(total_count) {
    list(
      "count_in_radius" = 0,
      "closest_distance_km" = NA,
      "mean_distance_km" = NA,
      "total_count_in_world" = total_count,
      "search_radius" = radius_km,
      "closest_point_lat" = NA,
      "closest_point_lon" = NA,
      "sample_point_lat" = latitude,
      "sample_point_lon" = longitude
    )
  }

  fetched <- fetch_occur_bbox(
    taxon_key = species_key,
    bbox = bbox,
    method = method,
    n_occur = n_occur,
    verbose = verbose,
    ...
  )
  occ_data <- fetched$data
  bbox_count <- fetched$count

  if (is.null(bbox_count) || bbox_count == 0) {
    if (verbose) {
      cli::cli_alert_warning("No occurrences found for {.emph {taxa_name}}")
    }
    return(empty_result(0))
  }

  if (clean_coord) {
    check_package("CoordinateCleaner")
    n_occur_old <- nrow(occ_data)
    occ_data <-
      CoordinateCleaner::clean_coordinates(
        occ_data,
        lon = "decimalLongitude",
        lat = "decimalLatitude",
        verbose = clean_coord_verbose,
        species = "scientificName"
      ) |>
      filter(.data$.summary)

    if (verbose) {
      remaining_occurrences <- nrow(occ_data)
      percentage <- round(100 * remaining_occurrences / n_occur_old, 1)
      cli::cli_alert_info(c(
        "After cleaning with CoordinateCleaner::clean_coordinates:\n",
        "  - {.val {remaining_occurrences}} occurrences remain(s)\n",
        "  - Total original: {.val {n_occur_old}}\n",
        "  - Retention rate: {.val {percentage}}%"
      ))
    }
  }

  if (!is.null(info_names)) {
    info_names_resolved <- info_names
    if (method != "search") {
      info_names_resolved[info_names_resolved == "country"] <- "countryCode"
    }
    occ_data <- occ_data |>
      select(any_of(info_names_resolved))
  }

  stats <- compute_occur_stats(
    occ_df = occ_data,
    longitude = longitude,
    latitude = latitude,
    radius_km = radius_km,
    circle_form = circle_form
  )

  if (stats$count_in_radius == 0) {
    if (verbose) {
      cli::cli_alert_warning("No valid occurrences for {.emph {taxa_name}}")
    }
    return(empty_result(occurrences_world_with_coordinate))
  }

  if (verbose) {
    cli::cli_bullets(c(
      "v" = "Found {.val {stats$count_in_radius}} occurrences for species {.emph {taxa_name}}:",
      "*" = "Closest occurrence: {.val {stats$closest_distance_km}} km"
    ))
  }

  result <- list(
    "count_in_radius" = stats$count_in_radius,
    "closest_distance_km" = stats$closest_distance_km,
    "mean_distance_km" = stats$mean_distance_km,
    "total_count_in_world" = occurrences_world_with_coordinate,
    "search_radius" = radius_km,
    "closest_point_lat" = stats$closest_point_lat,
    "closest_point_lon" = stats$closest_point_lon,
    "sample_point_lat" = latitude,
    "sample_point_lon" = longitude
  )

  if (return_all_occ) {
    result <- c(list("occ_data" = stats$occ_data), result)
  }

  result
}


#' Fetch GBIF occurrences for one taxon within a bounding box
#'
#' @inheritParams tax_occur_check
#' @param taxon_key (integer) GBIF usage key.
#' @param bbox (list) Bounding box with `xmin`, `xmax`, `ymin`, `ymax`.
#' @returns A list with `data` (occurrence data frame) and `count` (number of
#'  records in the bounding box).
#' @author Adrien Taudiere
#' @keywords internal
fetch_occur_bbox <- function(
  taxon_key,
  bbox,
  method = "download",
  n_occur = 1000,
  verbose = TRUE,
  ...
) {
  if (method == "search") {
    occ <- rgbif::occ_search(
      taxonKey = taxon_key,
      hasCoordinate = TRUE,
      hasGeospatialIssue = FALSE,
      decimalLongitude = paste(bbox$xmin, bbox$xmax, sep = ","),
      decimalLatitude = paste(bbox$ymin, bbox$ymax, sep = ","),
      limit = n_occur,
      ...
    )
    return(list(data = occ$data, count = occ$meta$count))
  }

  occ_data <- gbif_download(
    rgbif::pred("taxonKey", taxon_key),
    rgbif::pred("hasCoordinate", TRUE),
    rgbif::pred("hasGeospatialIssue", FALSE),
    rgbif::pred_gte("decimalLatitude", bbox$ymin),
    rgbif::pred_lte("decimalLatitude", bbox$ymax),
    rgbif::pred_gte("decimalLongitude", bbox$xmin),
    rgbif::pred_lte("decimalLongitude", bbox$xmax),
    verbose = verbose
  )

  if (!is.null(occ_data) && nrow(occ_data) > n_occur) {
    occ_data <- occ_data |> slice_sample(n = n_occur)
  }

  list(
    data = occ_data,
    count = if (is.null(occ_data)) {
      0
    } else {
      nrow(occ_data)
    }
  )
}


#' Bounding box covering several points, expanded by a radius
#'
#' @param longitudes,latitudes (numeric) Point coordinates in decimal degrees.
#' @param radius_km (numeric) Radius in kilometres to expand the box by.
#' @returns A list with `xmin`, `xmax`, `ymin`, `ymax`.
#' @author Adrien Taudiere
#' @keywords internal
bbox_for_points <- function(longitudes, latitudes, radius_km) {
  lat_offset <- radius_km / 111.32
  lon_offset <- radius_km /
    (111.32 * cos(max(abs(latitudes)) * pi / 180))
  list(
    xmin = min(longitudes) - lon_offset,
    xmax = max(longitudes) + lon_offset,
    ymin = min(latitudes) - lat_offset,
    ymax = max(latitudes) + lat_offset
  )
}


#' Fetch and attribute GBIF occurrences for several taxa at once
#'
#' @description
#' Shared back-end for [tax_occur_check_pq()] and [tax_occur_multi_check_pq()].
#' With `method = "download"`/`"download_sql"` it issues a **single** GBIF
#' download for all taxa (optionally constrained to `bbox`) and attributes each
#' record with [attribute_gbif_records()]. With `method = "search"` it falls
#' back to a per-taxon [rgbif::occ_search()] loop.
#'
#' @inheritParams tax_gbif_occur_coords
#' @param gbif_taxa (tibble) Resolved taxa with `usageKey`, `canonicalName` and
#'  (for `method = "search"`) the bounding-box constraint applied per taxon.
#' @param bbox (list or NULL) Optional bounding box (`xmin`/`xmax`/`ymin`/`ymax`)
#'  used as a server-side spatial filter.
#' @param clean_coord,clean_coord_verbose Passed to
#'  [CoordinateCleaner::clean_coordinates()].
#' @returns A data frame of attributed occurrences (`taxon_name`, `usageKey`,
#'  `decimalLongitude`, `decimalLatitude`, `scientificName`, ...), or `NULL`.
#' @author Adrien Taudiere
#' @keywords internal
fetch_occur_for_taxa <- function(
  gbif_taxa,
  method = "download",
  n_occur = 1000,
  bbox = NULL,
  clean_coord = TRUE,
  clean_coord_verbose = FALSE,
  verbose = TRUE
) {
  keys <- gbif_taxa$usageKey

  if (method == "search") {
    occ_list <- vector("list", nrow(gbif_taxa))
    for (i in seq_len(nrow(gbif_taxa))) {
      args <- list(
        taxonKey = keys[i],
        hasCoordinate = TRUE,
        hasGeospatialIssue = FALSE,
        limit = n_occur
      )
      if (!is.null(bbox)) {
        args$decimalLongitude <- paste(bbox$xmin, bbox$xmax, sep = ",")
        args$decimalLatitude <- paste(bbox$ymin, bbox$ymax, sep = ",")
      }
      res_i <- do.call(rgbif::occ_search, args)$data
      if (!is.null(res_i) && nrow(res_i) > 0) {
        res_i$taxon_name <- gbif_taxa$verbatim_name[i]
        res_i$usageKey <- keys[i]
        occ_list[[i]] <- res_i
      }
    }
    occ_data <- bind_rows(occ_list)
  } else {
    preds <- list(
      rgbif::pred_in("taxonKey", keys),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred("hasGeospatialIssue", FALSE)
    )
    if (!is.null(bbox)) {
      preds <- c(
        preds,
        list(
          rgbif::pred_gte("decimalLatitude", bbox$ymin),
          rgbif::pred_lte("decimalLatitude", bbox$ymax),
          rgbif::pred_gte("decimalLongitude", bbox$xmin),
          rgbif::pred_lte("decimalLongitude", bbox$xmax)
        )
      )
    }
    occ_data <- do.call(gbif_download, c(preds, list(verbose = verbose)))
    if (!is.null(occ_data) && nrow(occ_data) > 0) {
      occ_data <- attribute_gbif_records(occ_data, gbif_taxa)
    }
  }

  if (is.null(occ_data) || nrow(occ_data) == 0) {
    return(NULL)
  }

  if (clean_coord) {
    check_package("CoordinateCleaner")
    occ_data <- CoordinateCleaner::clean_coordinates(
      occ_data,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = "taxon_name",
      verbose = clean_coord_verbose
    ) |>
      filter(.data$.summary)
  }

  occ_data
}


#' Per-taxon occurrence statistics at one point from pre-fetched occurrences
#'
#' @inheritParams tax_occur_check
#' @param occ_all (data frame) Attributed occurrences for all taxa (output of
#'  [fetch_occur_for_taxa()]).
#' @param gbif_taxa (tibble) Resolved taxa with `usageKey` and `canonicalName`.
#' @param world_counts (numeric) Per-taxon worldwide georeferenced counts,
#'  aligned with the rows of `gbif_taxa`.
#' @returns A tibble with one row per taxon and the [tax_occur_check()] statistic
#'  columns.
#' @author Adrien Taudiere
#' @keywords internal
occur_check_compute_df <- function(
  occ_all,
  gbif_taxa,
  world_counts,
  longitude,
  latitude,
  radius_km,
  circle_form = TRUE
) {
  rows <- lapply(seq_len(nrow(gbif_taxa)), function(i) {
    key <- gbif_taxa$usageKey[i]
    if (is.null(occ_all)) {
      occ_i <- NULL
    } else {
      occ_i <- occ_all |> filter(.data$usageKey == key)
    }
    stats <- compute_occur_stats(
      occ_df = occ_i,
      longitude = longitude,
      latitude = latitude,
      radius_km = radius_km,
      circle_form = circle_form
    )
    tibble::tibble(
      taxa_name = gbif_taxa$canonicalName[i],
      count_in_radius = stats$count_in_radius,
      closest_distance_km = stats$closest_distance_km,
      mean_distance_km = stats$mean_distance_km,
      total_count_in_world = world_counts[i],
      search_radius = radius_km,
      closest_point_lat = stats$closest_point_lat,
      closest_point_lon = stats$closest_point_lon,
      sample_point_lat = latitude,
      sample_point_lon = longitude
    )
  })
  bind_rows(rows)
}
