# Single GBIF download for \[tax_gbif_occur_coords()\]

Single GBIF download for \[tax_gbif_occur_coords()\]

## Usage

``` r
gbif_occur_coords_download(
  gbif_taxa,
  n_occur,
  keep_cols,
  method = "download",
  country = NULL,
  year_gte = NULL,
  year_lte = NULL,
  geometry = NULL,
  verbose = TRUE
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

- country:

  (character, default \`NULL\`). Optional ISO2 country code used as a
  server-side filter for the download methods (e.g. \`"FR"\`).

- year_gte, year_lte:

  (numeric, default \`NULL\`). Optional inclusive year bounds used as
  server-side filters for the download methods.

- geometry:

  (character, default \`NULL\`). Optional WKT polygon used as a
  server-side spatial filter for \`method = "download"\` (via
  \[rgbif::pred_within()\]). Not supported with \`method =
  "download_sql"\`.

- verbose:

  (logical, default \`TRUE\`). If \`TRUE\`, print progress messages.

## Value

A tibble of occurrences with \`taxon_name\` and \`usageKey\`, or
\`NULL\`.

## Author

Adrien Taudiere
