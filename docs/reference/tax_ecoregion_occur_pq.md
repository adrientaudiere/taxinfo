# Count GBIF occurrences per ecoregion for the taxa of a phyloseq object

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Phyloseq wrapper around \[tax_ecoregion_occur()\]. Extracts taxon names
from \`physeq\` using the column(s) named in \`taxonomic_rank\` (default
\`"currentCanonicalSimple"\`, the output of \[gna_verifier_pq()\]; use
\`"genusSpeciesEpithet"\` to match the column produced by
\`gna_verifier_pq(..., genus_species_canonical_col = TRUE)\`), then
queries GBIF and maps occurrences to WWF/TNC terrestrial ecoregions.

## Usage

``` r
tax_ecoregion_occur_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
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

- add_to_phyloseq:

  (logical, default \`TRUE\` when \`physeq\` is provided, \`FALSE\`
  otherwise). If \`TRUE\`, add three columns
  (\`\<col_prefix\>ecoregion_top\`, \`\<col_prefix\>ecoregion_n\`,
  \`\<col_prefix\>ecoregion_list\`) to \`physeq@tax_table\` and return
  the updated phyloseq object. If \`FALSE\`, return the long tibble from
  \[tax_ecoregion_occur()\].

- col_prefix:

  (character, default \`NULL\`). Prefix for the new tax_table columns.
  Defaults to \`"ecoregion\_"\` if \`NULL\` (yielding \`ecoregion_top\`
  / \`ecoregion_n\` / \`ecoregion_list\`).

- n_occur:

  (numeric, default \`1000\`). Maximum number of occurrences to keep per
  taxon. With \`method = "search"\` this is a server-side limit; with
  the download methods it is applied as a local sample after import (a
  warning is issued when a taxon exceeded \`n_occur\`).

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
  \[rgbif::occ_search()\] calls to avoid GBIF rate-limiting. Only used
  when \`method = "search"\`.

- discard_genus_alone:

  (logical, default \`TRUE\` when \`taxonomic_rank ==
  "currentCanonicalSimple"\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- discard_NA:

  (logical, default \`TRUE\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

## Value

Either a phyloseq object with three new tax_table columns (if
\`add_to_phyloseq = TRUE\`) or the long tibble produced by
\[tax_ecoregion_occur()\] (otherwise). In the latter case,
\`attr(result, "tax_summary")\` holds the one-row-per-taxon summary used
to build the phyloseq columns.

## See also

\[tax_ecoregion_occur()\], \[tax_check_ecoregion()\],
\[taxonomic_rank_to_taxnames()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_clean <- gna_verifier_pq(data_fungi_mini)
tax_ecoregion_occur_pq(
  data_fungi_mini_clean,
  taxonomic_rank = "genusSpeciesEpithet",
  n_occur = 100
)
} # }
```
