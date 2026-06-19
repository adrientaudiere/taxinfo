# Taxa occurrences check within a radius using GBIF data

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Performs a species occurrence check within a fixed radius around a GPS
point using GBIF occurrence data.

## Usage

``` r
tax_occur_check(
  taxa_name,
  longitude,
  latitude,
  radius_km = 50,
  method = c("download", "search"),
  circle_form = TRUE,
  clean_coord = TRUE,
  info_names = c("decimalLongitude", "decimalLatitude", "country", "year",
    "scientificName", "recordedBy", "gbifRegion"),
  return_all_occ = FALSE,
  verbose = TRUE,
  clean_coord_verbose = FALSE,
  n_occur = 1000,
  ...
)
```

## Arguments

- taxa_name:

  Character. Scientific name of the species to check.

- longitude:

  Numeric. Longitude of the test point in decimal degrees.

- latitude:

  Numeric. Latitude of the test point in decimal degrees.

- radius_km:

  Numeric. Search radius in kilometers (default: 50).

- method:

  (character, default \`"download"\`). How occurrences are fetched: -
  \`"download"\`: a single \[rgbif::occ_download()\] request constrained
  to the search bounding box (mints a citable DOI). \*\*Requires GBIF
  credentials\*\* (see \[check_gbif_credentials()\]). - \`"search"\`:
  the legacy \[rgbif::occ_search()\] call (fast, capped at \`n_occur\`
  records, no credentials).

- circle_form:

  (Logical, default: TRUE). Whether to use a circular search area. If
  FALSE, a square bounding box is used.

- clean_coord:

  (Logical, default: TRUE). Whether to clean coordinates using
  CoordinateCleaner

- info_names:

  Character vector. Columns to select from GBIF data
  (default:c("decimalLongitude", "decimalLatitude", "country", "year",
  "scientificName", "recordedBy", "gbifRegion")). Note that
  "scientificName", "decimalLongitude" and "decimalLatitude" are
  required. With \`method = "download"\`, \`"country"\` is mapped to
  \`"countryCode"\` and download-only absent columns (e.g.
  \`"gbifRegion"\`) are silently dropped.

- return_all_occ:

  (Logical, default: FALSE). If TRUE, return all occurrences found
  within the radius in a data frame called "occ_data" in the resulting
  list.

- verbose:

  (Logical, default: TRUE). Whether to print progress messages.

- clean_coord_verbose:

  (Logical, default: FALSE). Whether to print messages from
  CoordinateCleaner.

- n_occur:

  Numeric (default: 1000). Maximum number of occurrences to retrieve
  from GBIF. A server-side limit with \`method = "search"\`; applied as
  a local sample after import with \`method = "download"\`.

- ...:

  Additional parameters passed to \[rgbif::occ_search()\] (only used
  when \`method = "search"\`).

## Value

A list containing: - count_in_radius: Number of occurrences found within
the radius - closest_distance_km: Distance to the closest occurrence in
kilometers - mean_distance_km: Mean distance to all occurrences in
kilometers - total_count_in_world: Total number of occurrences with
coordinates worldwide - search_radius: The search radius used (in
kilometers) - closest_point_lat: Latitude of the closest occurrence -
closest_point_lon: Longitude of the closest occurrence -
sample_point_lat: Latitude of the tested point - sample_point_lon:
Longitude of the tested point - occ_data (optional, if
\`return_all_occ\` is TRUE): Data frame of all occurrences found within
the radius

## See also

\[tax_occur_check_pq()\], \[tax_occur_multi_check_pq()\],
\[rgbif::occ_download()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
# Check for Oak species near Paris
long <- 2.3522
lat <- 48.8566

Q_rob_in_Paris <- tax_occur_check("Quercus robur", long, lat, radius_km=10)
Q_rob_in_Paris

tax_occur_check("Trametopsis brasiliensis", long, lat, radius_km=100)


# Visualize occurrences around Paris for Fagus sylvatica
res_occ <- tax_occur_check("Fagus sylvatica", long, lat, radius_km=20,
  return_all_occ = TRUE
)

occ_data_sf <- sf::st_as_sf(res_occ$occ_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

if (requireNamespace("leaflet")) {
  library(leaflet)
}
if (requireNamespace("leafpop")) {
  library(leafpop)
}
leaflet() |>
  addTiles() |>
  setView(lat, long, zoom = 12) |>
  fitBounds(
    lat1 = as.vector(sf::st_bbox(occ_data_sf))[2],
    lng1 = as.vector(sf::st_bbox(occ_data_sf))[1],
    lat2 = as.vector(sf::st_bbox(occ_data_sf))[4],
    lng2 = as.vector(sf::st_bbox(occ_data_sf))[3]
  ) |>
  leaflet::addCircles(data = occ_data_sf, color = "blue", stroke = 1, opacity = 0.8) |>
  leaflet::addCircleMarkers(lat, long, color = "orange", radius = 2, opacity = 1)
} # }
```
