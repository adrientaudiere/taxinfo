# Per-taxon \`occ_search\` loop for \[tax_gbif_occur_coords()\]

Per-taxon \`occ_search\` loop for \[tax_gbif_occur_coords()\]

## Usage

``` r
gbif_occur_coords_search(
  gbif_taxa,
  n_occur,
  keep_cols,
  verbose = TRUE,
  time_to_sleep = 0.3
)
```

## Arguments

- gbif_taxa:

  (tibble) Resolved GBIF taxa with \`usageKey\`, \`canonicalName\` and
  \`verbatim_name\`.

- n_occur:

  (numeric, default \`1000\`). Maximum number of occurrences to keep per
  taxon. With \`method = "search"\` this is a server-side limit; with
  the download methods it is applied as a local sample after import (a
  warning is issued when a taxon exceeded \`n_occur\`).

- keep_cols:

  (character) Occurrence columns to retain.

- verbose:

  (logical, default \`TRUE\`). If \`TRUE\`, print progress messages.

- time_to_sleep:

  (numeric, default \`0.3\`). Seconds to pause between
  \[rgbif::occ_search()\] calls to avoid GBIF rate-limiting. Only used
  when \`method = "search"\`.

## Value

A tibble of occurrences with \`taxon_name\` and \`usageKey\`, or
\`NULL\`.

## Author

Adrien Taudiere
