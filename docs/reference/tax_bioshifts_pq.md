# Add range-shift information from the BioShifts database to a phyloseq object

\<a
href="https://adrientaudiere.github.io/taxinfo/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Query the BioShifts database (a global compilation of documented species
range shifts under climate change) through the \`BioShiftR\` package and
attach, for each taxon of a \`phyloseq\` object, a summary of the
observed range-shift rates. Latitudinal (\`"LAT"\`) and elevational
(\`"ELE"\`) shifts are summarized separately as the mean shift rate
across all matching records, together with the number of records found.

Species are matched by name between the \`taxonomic_rank\` column(s) of
the \`tax_table\` and the \`sp_name_checked\` column returned by
\`BioShiftR::get_shifts()\`. By default the BioShifts names are first
harmonized with \[gna_verifier_pq()\] and the join is performed on the
resulting \`currentCanonicalSimple\` names, so that synonyms and
orthographic variants on the BioShifts side are reconciled with the
(already GNA-verified) names of the \`phyloseq\` object. Set
\`skip_name_verification = TRUE\` to skip this correction and match the
raw \`sp_name_checked\` values directly. Matching is always case- and
separator-insensitive, so a single \`"Genus_species"\` column
(underscores) matches BioShifts' \`"Genus species"\` (spaces).

## Usage

``` r
tax_bioshifts_pq(
  physeq,
  taxonomic_rank = "currentCanonicalSimple",
  group = "All",
  eco = "All",
  continent = "All",
  type = c("LAT", "ELE"),
  skip_name_verification = FALSE,
  data_sources = c(1, 12),
  shifts_data = NULL,
  col_prefix = "bioshift_",
  add_to_phyloseq = TRUE,
  verbose = TRUE
)
```

## Arguments

- physeq:

  (required) A
  [`phyloseq-class`](https://rdrr.io/pkg/phyloseq/man/phyloseq-class.html)
  object with a \`tax_table\` containing the \`taxonomic_rank\`
  column(s).

- taxonomic_rank:

  (character, default \`"currentCanonicalSimple"\`) The \`tax_table\`
  column(s) holding the taxon name used for matching. May be a single
  column (typically the GNA-verified \`currentCanonicalSimple\`, see
  \[gna_verifier_pq()\], or a \`"Genus_species"\` binomial column) or a
  vector of columns pasted together in order (e.g. \`c("Genus",
  "Species")\` when the genus and the species epithet are stored
  separately).

- group, eco, continent, type:

  Filters passed to \`BioShiftR::get_shifts()\`. \`type\` is the shift
  dimension(s) to summarize and accepts any subset of \`c("LAT",
  "ELE")\`. See \`?BioShiftR::get_shifts\`.

- skip_name_verification:

  (logical, default \`FALSE\`) If \`FALSE\` (the default), the BioShifts
  \`sp_name_checked\` names are harmonized with \[gna_verifier_pq()\]
  and matched to the \`phyloseq\` names on \`currentCanonicalSimple\`.
  If \`TRUE\`, this correction is skipped and the raw
  \`sp_name_checked\` values are matched directly. Set to \`TRUE\` for
  offline use or when the names are already known to be consistent.

- data_sources:

  (numeric, default \`c(1, 12)\`) Data sources passed to
  \[gna_verifier_pq()\] when \`skip_name_verification = FALSE\`.

- shifts_data:

  (optional data.frame) A pre-fetched \`BioShiftR::get_shifts()\` result
  (with columns \`sp_name_checked\`, \`type\` and \`calc_rate\`). When
  supplied, the live query is skipped and \`group\` / \`eco\` /
  \`continent\` / \`type\` are only used to select which \`type\`s to
  summarize. Useful for reproducible analyses and tests.

- col_prefix:

  (character, default \`"bioshift\_"\`) Prefix applied to the added
  columns (\`bioshift_LAT_rate\`, \`bioshift_ELE_rate\`,
  \`bioshift_n_records\`).

- add_to_phyloseq:

  (logical, default \`TRUE\`) If \`TRUE\`, return the phyloseq object
  with the new columns added to its \`tax_table\`; if \`FALSE\`, return
  the augmented \`tax_table\` as a tibble.

- verbose:

  (logical, default \`TRUE\`) Print a summary message.

## Value

Either an updated \`phyloseq\` object (when \`add_to_phyloseq = TRUE\`)
or a tibble of the augmented \`tax_table\` (when \`add_to_phyloseq =
FALSE\`).

## References

Comte, L., Bertrand, R., Diamond, S., Lenoir, J. et al. (2024)
Mechanisms, detection and impacts of species redistributions under
climate change. Nature Reviews Earth & Environment.

Lenoir, J., Bertrand, R., Comte, L. et al. (2020) Species better track
climate warming in the oceans than on land. Nature Ecology & Evolution
4, 1044-1059.

Data are queried through the in-development \`BioShiftR\` package
(<https://bioshifts.github.io/BioShiftR/>), which serves the current
BioShifts release: the original BioShifts merged with the CoRE database
(Rubenstein et al.), amounting to roughly 31,760 range-shift estimates
for about 12,912 species across marine, freshwater and terrestrial
ecosystems.

## See also

[`tax_metatraits_pq`](https://adrientaudiere.github.io/taxinfo/reference/tax_metatraits_pq.md),
[`gna_verifier_pq`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md)

## Author

Adrien Taudière

## Examples

``` r
if (FALSE) { # \dontrun{
# Live query (needs the BioShiftR package and network access). By default the
# BioShifts names are GNA-verified and matched on `currentCanonicalSimple`,
# so run `gna_verifier_pq()` on your phyloseq first:
data(data_fungi_mini, package = "MiscMetabar")
pq <- gna_verifier_pq(data_fungi_mini)
pq <- tax_bioshifts_pq(pq, group = "FUNGI", type = "LAT")

# `data_fungi_mini` also ships a single `Genus_species` (underscore) column,
# which matches BioShifts directly:
data_fungi_mini2 <- data_fungi_mini
data_fungi_mini2@tax_table[1:3, "Genus_species"] <- c(
  "Ramalina_farinacea", "Evernia_prunastri", "Sphaerophorus_fragilis"
)
pq2 <- tax_bioshifts_pq(data_fungi_mini2, taxonomic_rank = "Genus_species")
} # }

# Offline: supply a pre-fetched shifts table and skip name verification.
shifts <- data.frame(
  sp_name_checked = c("Genusa speciesa", "Genusa speciesa", "Genusb speciesb"),
  type = c("LAT", "LAT", "ELE"),
  calc_rate = c(1.2, 0.8, -3.5),
  stringsAsFactors = FALSE
)
otu <- matrix(1, nrow = 2, ncol = 1, dimnames = list(c("t1", "t2"), "s1"))
tax <- matrix(
  c("Genusa", "speciesa", "Genusb", "speciesb"),
  nrow = 2, byrow = TRUE, dimnames = list(c("t1", "t2"), c("Genus", "Species"))
)
pq <- phyloseq::phyloseq(
  phyloseq::otu_table(otu, taxa_are_rows = TRUE),
  phyloseq::tax_table(tax)
)
tax_bioshifts_pq(
  pq,
  taxonomic_rank = c("Genus", "Species"),
  shifts_data = shifts,
  skip_name_verification = TRUE,
  add_to_phyloseq = FALSE
)
#> Matched BioShifts records for 2/2 taxa.
#> # A tibble: 2 × 5
#>   Genus  Species  bioshift_LAT_rate bioshift_ELE_rate bioshift_n_records
#>   <chr>  <chr>                <dbl>             <dbl>              <int>
#> 1 Genusa speciesa                 1              NA                    2
#> 2 Genusb speciesb                NA              -3.5                  1
```
