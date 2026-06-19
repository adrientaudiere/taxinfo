# Fetch GBIF occurrences for one taxon within a bounding box

Fetch GBIF occurrences for one taxon within a bounding box

## Usage

``` r
fetch_occur_bbox(
  taxon_key,
  bbox,
  method = "download",
  n_occur = 1000,
  verbose = TRUE,
  ...
)
```

## Arguments

- taxon_key:

  (integer) GBIF usage key.

- bbox:

  (list) Bounding box with \`xmin\`, \`xmax\`, \`ymin\`, \`ymax\`.

- method:

  (character, default \`"download"\`). How occurrences are fetched: -
  \`"download"\`: a single \[rgbif::occ_download()\] request constrained
  to the search bounding box (mints a citable DOI). \*\*Requires GBIF
  credentials\*\* (see \[check_gbif_credentials()\]). - \`"search"\`:
  the legacy \[rgbif::occ_search()\] call (fast, capped at \`n_occur\`
  records, no credentials).

- n_occur:

  Numeric (default: 1000). Maximum number of occurrences to retrieve
  from GBIF. A server-side limit with \`method = "search"\`; applied as
  a local sample after import with \`method = "download"\`.

- verbose:

  (Logical, default: TRUE). Whether to print progress messages.

- ...:

  Additional parameters passed to \[rgbif::occ_search()\] (only used
  when \`method = "search"\`).

## Value

A list with \`data\` (occurrence data frame) and \`count\` (number of
records in the bounding box).

## Author

Adrien Taudiere
