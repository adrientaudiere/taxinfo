# Fetch GBIF occurrences per taxon until its ecoregion set saturates

Fetch GBIF occurrences per taxon until its ecoregion set saturates

## Usage

``` r
ecoregion_saturation_fetch(
  taxnames = NULL,
  gbif_taxa = NULL,
  n_occur = 1000,
  batch_size = 250,
  min_points = 100,
  patience = 2,
  clean_coord = FALSE,
  time_to_sleep = 0.3,
  verbose = TRUE,
  fetch_page = NULL,
  key_fun = NULL
)
```

## Arguments

- taxnames:

  (character) Query names. Ignored when \`gbif_taxa\` is supplied.

- gbif_taxa:

  (tibble or \`NULL\`) Resolved GBIF taxa with columns \`usageKey\` and
  \`verbatim_name\` (as returned by
  \[rgbif::name_backbone_checklist()\]). When \`NULL\`, \`taxnames\` are
  resolved through the GBIF backbone (network).

- n_occur:

  (numeric) Hard cap on the number of occurrences fetched per taxon.

- batch_size:

  (numeric) Page size of each \`fetch_page\` call.

- min_points:

  (numeric) Minimum number of points fetched per taxon before the
  saturation rule may fire.

- patience:

  (numeric) Consecutive batches without a new ecoregion tolerated before
  stopping.

- clean_coord:

  (logical) If \`TRUE\`, run \[CoordinateCleaner::clean_coordinates()\]
  on the result.

- time_to_sleep:

  (numeric) Courtesy delay between GBIF calls (used by the default
  \`fetch_page\`).

- verbose:

  (logical) Progress messages.

- fetch_page:

  (function or \`NULL\`) \`function(taxon_key, limit, start)\` returning
  one page of occurrences. Defaults to \[gbif_occ_search_page()\].

- key_fun:

  (function or \`NULL\`) \`function(batch)\` returning the grouping keys
  (ecoregion names) of one batch of points. Defaults to a WWF/TNC lookup
  through \[points_to_ecoregions()\].

## Value

A tibble of occurrences with \`taxon_name\` and \`usageKey\`, with
\`attr(, "missing_taxa")\` (query names without any occurrence) and
\`attr(, "saturation")\` (one row per fetched taxon: \`taxon_name\`,
\`n_points\`, \`n_ecoregions\`, \`n_batches\`, \`status\` in
\`c("saturated", "exhausted", "cap")\`).

## Author

Adrien Taudiere
