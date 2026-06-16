# Get GBIF occurrence coordinates for a vector of taxa

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Retrieves up to \`n_occur\` georeferenced GBIF occurrences for each name
in \`taxnames\` and returns them as a long tibble. Taxa are resolved to
GBIF usage keys once via \[rgbif::name_backbone_checklist()\] (filtering
on \`matchType with \[rgbif::occ_search()\] (\`hasGeospatialIssue =
FALSE\`). Rows with missing coordinates are dropped.

## Usage

``` r
tax_gbif_occur_coords(
  taxnames,
  n_occur = 1000,
  clean_coord = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3
)
```

## Arguments

- taxnames:

  (character vector) Scientific names of the taxa to query.

- n_occur:

  (numeric, default \`1000\`). Maximum number of occurrences to retrieve
  per taxon. Use a smaller value (e.g. \`200\`) for quick checks.

- clean_coord:

  (logical, default \`FALSE\`). If \`TRUE\`, run
  \[CoordinateCleaner::clean_coordinates()\] on the result (requires the
  \`CoordinateCleaner\` package).

- verbose:

  (logical, default \`TRUE\`). If \`TRUE\`, print progress messages.

- time_to_sleep:

  (numeric, default \`0.3\`). Seconds to pause between
  \[rgbif::occ_search()\] calls to avoid GBIF rate-limiting.

## Value

A tibble with columns \`taxon_name\`, \`usageKey\`,
\`decimalLongitude\`, \`decimalLatitude\`, \`countryCode\`, \`year\`,
\`gbifID\`. Taxa with zero valid occurrences are listed in
\`attr(result, "missing_taxa")\`.

## See also

\[tax_ecoregion_occur()\], \[rgbif::occ_search()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
tax_gbif_occur_coords(
  c("Xylobolus subpileatus", "Amanita muscaria"),
  n_occur = 200
)
} # }
```
