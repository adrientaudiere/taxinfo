# Per-taxon occurrence statistics at one point from pre-fetched occurrences

Per-taxon occurrence statistics at one point from pre-fetched
occurrences

## Usage

``` r
occur_check_compute_df(
  occ_all,
  gbif_taxa,
  world_counts,
  longitude,
  latitude,
  radius_km,
  circle_form = TRUE
)
```

## Arguments

- occ_all:

  (data frame) Attributed occurrences for all taxa (output of
  \[fetch_occur_for_taxa()\]).

- gbif_taxa:

  (tibble) Resolved taxa with \`usageKey\` and \`canonicalName\`.

- world_counts:

  (numeric) Per-taxon worldwide georeferenced counts, aligned with the
  rows of \`gbif_taxa\`.

- longitude:

  Numeric. Longitude of the test point in decimal degrees.

- latitude:

  Numeric. Latitude of the test point in decimal degrees.

- radius_km:

  Numeric. Search radius in kilometers (default: 50).

- circle_form:

  (Logical, default: TRUE). Whether to use a circular search area. If
  FALSE, a square bounding box is used.

## Value

A tibble with one row per taxon and the \[tax_occur_check()\] statistic
columns.

## Author

Adrien Taudiere
