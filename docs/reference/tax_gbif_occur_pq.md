# Get number of occurrences for each taxa of a phyloseq object

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

A wrapper of \[rgbif::occ_search()\] function to get the number of
occurences. Optionally, the number of occurrences can be obtained by
years or by country.

## Usage

``` r
tax_gbif_occur_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  by_country = FALSE,
  by_years = FALSE,
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

  (Character, default "currentCanonicalSimple") The column(s) present in
  the @tax_table slot of the phyloseq object. Can be a vector of two
  columns (e.g. c("Genus", "Species")).

- add_to_phyloseq:

  (logical, default TRUE when physeq is provided, FALSE when taxnames is
  provided) If TRUE, add new column(s) in the tax_table of the phyloseq
  object. Automatically set to TRUE when a phyloseq object is provided
  and FALSE when taxnames is provided. Cannot be TRUE if \`taxnames\` is
  provided.

- col_prefix:

  A character string to be added as a prefix to the new columns names
  added to the tax_table slot of the phyloseq object (default: NULL).

- by_country:

  (logical, default FALSE) If TRUE, the number of occurences is computed
  by country

- by_years:

  (logical, default FALSE) If TRUE, the number of occurences is computed
  by years

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- time_to_sleep:

  (numeric, default 0.3) Time to sleep between two calls to
  rgbif::occ_search(). Useful to avoid to be blocked by GBIF. Try to
  increase this value if you are blocked by the error "To download GBIF
  occurrence data in bulk, please request..."

- discard_genus_alone:

  (logical, default \`TRUE\` when \`taxonomic_rank ==
  "currentCanonicalSimple"\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- discard_NA:

  (logical, default \`TRUE\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

## Value

Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq object,
if add_to_phyloseq = TRUE, with new column(s) in the tax_table.

## Details

This function is mainly a wrapper of the work of others. Please cite
\`rgbif\` package.

## See also

\[rgbif::occ_search()\], \[plot_tax_gbif_pq()\], \[tax_occurr_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_cleanNames <-
  gna_verifier_pq(data_fungi_mini)

data_fungi_mini_cleanNames <- tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_country = TRUE)

# Get data without adding to phyloseq
tax_gbif_occur_pq(data_fungi_mini_cleanNames, add_to_phyloseq = FALSE)
tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_years = TRUE, add_to_phyloseq = FALSE)

# Using taxnames vector (returns a tibble)
tax_gbif_occur_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
ggplot(
  data_fungi_mini_cleanNames@tax_table,
  aes(y = log10(as.numeric(Global_occurences)), x = currentCanonicalSimple)
) +
  geom_col() +
  geom_col(aes(y = -log10(as.numeric(FR))), fill = "blue") +
  coord_flip() +
  xlab("Number of occurences (log10 scale) at global (grey) scale and in France (blue)")
} # }
```
