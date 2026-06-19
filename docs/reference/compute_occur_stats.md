# Compute occurrence statistics around a point

Pure (network-free) helper used by \[tax_occur_check()\] and its batched
wrappers. Given a data frame of occurrences with \`decimalLongitude\` /
\`decimalLatitude\` columns and a reference point, it computes the
distance of each occurrence to the point and summarises how many fall
within \`radius_km\`.

## Usage

``` r
compute_occur_stats(occ_df, longitude, latitude, radius_km, circle_form = TRUE)
```

## Arguments

- occ_df:

  (data frame) Occurrences with \`decimalLongitude\` and
  \`decimalLatitude\` columns. May be \`NULL\` or empty.

- longitude, latitude:

  (numeric) Reference point in decimal degrees.

- radius_km:

  (numeric) Search radius in kilometres.

- circle_form:

  (logical, default \`TRUE\`). If \`TRUE\`, keep only occurrences within
  \`radius_km\` of the point (circular area); if \`FALSE\`, all
  occurrences in \`occ_df\` are counted.

## Value

A list with \`count_in_radius\`, \`closest_distance_km\`,
\`mean_distance_km\`, \`closest_point_lat\`, \`closest_point_lon\` and
the (filtered) \`occ_data\`.

## Author

Adrien Taudiere
