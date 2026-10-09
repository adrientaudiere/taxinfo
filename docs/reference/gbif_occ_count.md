# Checklist-aware GBIF occurrence count

\[rgbif::occ_count()\] does not forward a \`checklistKey\` to GBIF. When
\`taxon_key\` belongs to the default checklist of the installed rgbif
the call is a plain \[rgbif::occ_count()\]; otherwise the count is read
from an equivalent \[rgbif::occ_search()\] call (\`limit = 0\`,
\`occurrenceStatus = "PRESENT"\`) carrying the \`checklistKey\` of
\`taxon_key\` (see \[gbif_occ_checklist_args()\]).

## Usage

``` r
gbif_occ_count(taxon_key, ...)
```

## Arguments

- taxon_key:

  A GBIF taxon key.

- ...:

  Other occurrence filters (e.g. \`hasCoordinate = TRUE\`).

## Value

The number of occurrence records (numeric).

## Author

Adrien Taudiere
