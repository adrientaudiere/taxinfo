# Add metaTraits phenotypic traits to a phyloseq object

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Augments the \`tax_table\` slot of a phyloseq object with harmonised
microbial phenotypic traits from the metaTraits resource (Robbani et al.
2026, \<https://metatraits.embl.de\>). metaTraits integrates
culture-derived trait information (BacDive, BV-BRC, JGI IMG, GOLD) with
genome-based predictions over GTDB r220, covering more than 140 traits
(cell morphology, motility, sporulation, oxygen/temperature/pH/salinity
preferences, metabolism, ...).

Traits are matched on GTDB taxon names. Matching is done species-first:
when a taxon's \`Species\` name is present in the species-level summary
its traits are used, and any trait missing at the species level falls
back to the genus-level summary. The (large) summary tables are
downloaded once and cached in a per-user directory
(\`tools::R_user_dir("taxinfo", "cache")\`).

## Usage

``` r
tax_metatraits_pq(
  physeq,
  taxonomic_rank = c("Genus", "Species"),
  level = c("species", "genus"),
  traits = NULL,
  groups = NULL,
  min_consensus_percentage = 0,
  taxonomy = "gtdb",
  no_predictions = FALSE,
  col_prefix = "mt_",
  add_to_phyloseq = TRUE,
  cache_dir = tools::R_user_dir("taxinfo", "cache"),
  refresh = FALSE,
  verbose = TRUE
)
```

## Arguments

- physeq:

  (required) A phyloseq object.

- taxonomic_rank:

  (Character, default \`c("Genus", "Species")\`) Column(s) of
  \`tax_table\` holding the GTDB names. The first element is the genus
  column; the (optional) second element is the species column. Supply a
  single genus column (e.g. \`"Genus"\`) to match at the genus level
  only.

- level:

  (Character vector, default \`c("species", "genus")\`) Taxonomic levels
  to query, in order of preference. Use \`"genus"\` alone to skip the
  large (~140 MB) species download.

- traits:

  (Character vector or \`NULL\`) Trait names (metaTraits \`trait_name\`)
  to keep. \`NULL\` (default) keeps every trait found for the matched
  taxa.

- groups:

  (Character vector or \`NULL\`) If supplied, keep only traits whose
  metaTraits \`group_1\` category is in this vector (e.g.
  \`"Metabolism"\`, \`"Environmental preferences"\`). Applied on top of
  \`traits\`.

- min_consensus_percentage:

  (Numeric, default \`0\`) Drop trait values whose
  \`consensus_percentage\` is below this threshold (set them to \`NA\`).

- taxonomy:

  (Character, default \`"gtdb"\`) Taxonomy of the summary files. Only
  \`"gtdb"\` supports name-based joins and is currently implemented.

- no_predictions:

  (Logical, default \`FALSE\`) If \`TRUE\`, use the culture-based-only
  summary files (without genome-based predictions).

- col_prefix:

  (Character, default \`"mt\_"\`) Prefix applied to all trait columns
  added to the \`tax_table\`.

- add_to_phyloseq:

  (Logical, default \`TRUE\`) If \`TRUE\`, return an updated phyloseq
  object. If \`FALSE\`, return a tibble of the augmented \`tax_table\`.

- cache_dir:

  (Character) Directory used to cache the downloaded summary files.
  Defaults to \`tools::R_user_dir("taxinfo", "cache")\`.

- refresh:

  (Logical, default \`FALSE\`) If \`TRUE\`, re-download the summary
  files even if a cached copy exists.

- verbose:

  (Logical, default \`TRUE\`) If \`TRUE\`, print progress messages.

## Value

Either an updated phyloseq object (when \`add_to_phyloseq = TRUE\`) or a
tibble of the augmented \`tax_table\`. A \`mt_trait_level\` column
records whether each taxon's traits came from the \`"species"\` or
\`"genus"\` summary (\`NA\` when unmatched).

## References

Podlesny et al. (2026). metaTraits: a large-scale integration of
microbial phenotypic trait information. \*Nucleic Acids Research\*,
[doi:10.1093/nar/gkaf1241](https://doi.org/10.1093/nar/gkaf1241)

## See also

\[tax_faprotax_pq()\], \[fungal_traits_guilds()\], \[tax_info_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
# GlobalPatterns is a bacterial/archaeal 16S dataset shipped with phyloseq.
# metaTraits joins on GTDB names, so coverage is highest when the tax_table
# already carries GTDB taxonomy (see e.g. tax_harmonize_backbone_pq()).
data(GlobalPatterns, package = "phyloseq")

# Genus + species matching (downloads ~40 MB + ~140 MB once, then cached)
res <- tax_metatraits_pq(GlobalPatterns)
table(res@tax_table[, "mt_trait_level"], useNA = "always")

# Genus only, restricted to metabolism traits (no species download)
res_g <- tax_metatraits_pq(GlobalPatterns, level = "genus", groups = "Metabolism")
table(res_g@tax_table[, "mt_oxygen preference"], useNA = "always")

# Return a tibble instead of a phyloseq object
tib <- tax_metatraits_pq(GlobalPatterns, level = "genus", add_to_phyloseq = FALSE)
} # }
```
