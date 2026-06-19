# Fetch and attribute GBIF occurrences for several taxa at once

Shared back-end for \[tax_occur_check_pq()\] and
\[tax_occur_multi_check_pq()\]. With \`method =
"download"\`/\`"download_sql"\` it issues a \*\*single\*\* GBIF download
for all taxa (optionally constrained to \`bbox\`) and attributes each
record with \[attribute_gbif_records()\]. With \`method = "search"\` it
falls back to a per-taxon \[rgbif::occ_search()\] loop.

## Usage

``` r
fetch_occur_for_taxa(
  gbif_taxa,
  method = "download",
  n_occur = 1000,
  bbox = NULL,
  clean_coord = TRUE,
  clean_coord_verbose = FALSE,
  verbose = TRUE
)
```

## Arguments

- gbif_taxa:

  (tibble) Resolved taxa with \`usageKey\`, \`canonicalName\` and (for
  \`method = "search"\`) the bounding-box constraint applied per taxon.

- method:

  (character, default \`"download"\`). How occurrences are fetched: -
  \`"download"\`: a single \[rgbif::occ_download()\] request for all
  taxa at once (no 100,000-record cap, mints a citable DOI).
  \*\*Requires GBIF credentials\*\* (see
  \[check_gbif_credentials()\]). - \`"download_sql"\`:
  \[rgbif::occ_download_sql()\] with server-side column selection and
  \`WHERE\` filtering (gated preview, must be enabled for your account).
  \*\*Requires GBIF credentials.\*\* Because GBIF SQL \`taxonkey\` is
  not hierarchical, this method matches \`taxonkey\`/\`specieskey\`
  directly and may under-return records for names matched at a higher
  rank (\`HIGHERRANK\`); use \`"download"\` if you need full
  hierarchical coverage. - \`"search"\`: the legacy per-taxon
  \[rgbif::occ_search()\] loop (fast, capped at 100,000 records, no
  credentials).

- n_occur:

  (numeric, default \`1000\`). Maximum number of occurrences to keep per
  taxon. With \`method = "search"\` this is a server-side limit; with
  the download methods it is applied as a local sample after import (a
  warning is issued when a taxon exceeded \`n_occur\`).

- bbox:

  (list or NULL) Optional bounding box
  (\`xmin\`/\`xmax\`/\`ymin\`/\`ymax\`) used as a server-side spatial
  filter.

- clean_coord, clean_coord_verbose:

  Passed to \[CoordinateCleaner::clean_coordinates()\].

- verbose:

  (logical, default \`TRUE\`). If \`TRUE\`, print progress messages.

## Value

A data frame of attributed occurrences (\`taxon_name\`, \`usageKey\`,
\`decimalLongitude\`, \`decimalLatitude\`, \`scientificName\`, ...), or
\`NULL\`.

## Author

Adrien Taudiere
