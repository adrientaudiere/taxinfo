# Check whether GPS points fall in ecoregions occupied by a set of taxa

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

For each name in \`taxnames\` (or for each taxon of a \`physeq\`
object), checks whether a set of test GPS points lie within a WWF/TNC
terrestrial ecoregion that is present in the taxon's GBIF range. The
function is a thin comparison wrapper around \[tax_ecoregion_occur()\]
(for the taxa) and \[points_to_ecoregions()\] (for the test points).

## Usage

``` r
tax_check_ecoregion(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  longitudes,
  latitudes,
  n_occur = 1000,
  min_nb_occur = 0,
  min_proportion = 0,
  clean_coord = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE
)
```

## Arguments

- physeq:

  (optional) A phyloseq object. Either \`physeq\` or \`taxnames\` must
  be provided, but not both.

- taxnames:

  (optional) A character vector of taxonomic names.

- taxonomic_rank:

  (character, default \`"currentCanonicalSimple"\`). The column(s) of
  \`physeq@tax_table\` to paste together as taxon names.

- longitudes:

  (numeric vector) Longitudes of the points to test.

- latitudes:

  (numeric vector) Latitudes of the points to test. Must have the same
  length as \`longitudes\`.

- n_occur:

  (numeric, default \`1000\`). Maximum number of occurrences to retrieve
  per taxon. Use a smaller value (e.g. \`200\`) for quick checks.

- min_nb_occur:

  (numeric, default \`0\`). Keep only (taxon, ecoregion) pairs with at
  least this many occurrences.

- min_proportion:

  (numeric, default \`0\`). Keep only (taxon, ecoregion) pairs whose
  share of the taxon's total occurrences is \`\>= min_proportion\` (a
  number in \`\[0, 1\]\`). Combined with \`min_nb_occur\` via AND.

- clean_coord:

  (logical, default \`FALSE\`). If \`TRUE\`, run
  \[CoordinateCleaner::clean_coordinates()\] on the result (requires the
  \`CoordinateCleaner\` package).

- verbose:

  (logical, default \`TRUE\`). If \`TRUE\`, print progress messages.

- time_to_sleep:

  (numeric, default \`0.3\`). Seconds to pause between
  \[rgbif::occ_search()\] calls to avoid GBIF rate-limiting.

- discard_genus_alone:

  (logical, default \`TRUE\` when \`taxonomic_rank ==
  "currentCanonicalSimple"\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- discard_NA:

  (logical, default \`TRUE\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

## Value

A list with four elements: - \`taxon_ecoregions\`: the long tibble
produced by \[tax_ecoregion_occur()\]. - \`points_ecoregion\`: the
tibble produced by \[points_to_ecoregions()\]. - \`is_in_ecoregion\`: a
logical matrix with rownames = taxon names and colnames =
\`"point\_\<i\>"\`, shape \`n_taxa x n_points\`. \`TRUE\` means the
ecoregion of the point is among the taxon's ecoregions that pass
\`min_nb_occur\` / \`min_proportion\`. - \`ecoregion\`: a named list
(one named integer vector per taxon) kept for backward compatibility
with earlier versions; prefer \`taxon_ecoregions\`.

## Details

The previous positional signature \`tax_check_ecoregion(taxa_name, lon,
lat)\` is no longer supported: the first argument is now \`physeq\`. Use
\`tax_check_ecoregion(taxnames = "Sp.", longitudes = lon, latitudes =
lat)\` for single-species calls.

## See also

\[tax_ecoregion_occur()\], \[tax_ecoregion_occur_pq()\],
\[points_to_ecoregions()\], \[tax_occur_check()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
requireNamespace("rgbif")
res <- tax_check_ecoregion(
  taxnames = "Xylobolus subpileatus",
  longitudes = c(2.3522, 4.2),
  latitudes  = c(48.8566, 33),
  n_occur = 200
)
res$is_in_ecoregion
} # }
```
