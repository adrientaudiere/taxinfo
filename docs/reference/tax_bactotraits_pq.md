# Add BactoTraits functional traits to a phyloseq object

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Augments the \`tax_table\` slot of a bacterial (or archaeal) phyloseq
object with functional traits from the BactoTraits database (Laderriere
et al. 2026, \<https://doi.org/10.1038/s41597-026-06652-2\>; data at
\<https://doi.org/10.24396/ORDAR-182\>, CC-BY). BactoTraits describes 31
functional traits (morphology, physiology, metabolism, genome
properties) for 97 721 bacterial strains as \*fuzzy-coded\* modality
columns: one column per \`trait_modality\` pair (e.g. \`motility_yes\`,
\`motility_no\`) holding the affinity score of the strain (or taxon) for
that modality, so that the modalities of a trait sum to one.

Matching is taxon-first with a rank fallback (as in the original
BactoTraits scripts): the most precise level that can be assigned wins
(\`species\` \> \`genus\` \> \`family\` \> \`order\` \> \`class\` \>
\`phylum\`), and the trait profile of coarser levels is the
strain-weighted mean of the genus rows of that level. The column
\`trait_level\` records where the profile comes from.

## Usage

``` r
tax_bactotraits_pq(
  physeq,
  file_name_species = NULL,
  file_name_genus = NULL,
  taxonomic_rank = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
  level = c("species", "genus", "family", "order", "class", "phylum"),
  merge_modalities = TRUE,
  positive_modalities = c("yes", "positive", "present", "true"),
  raw_cols = FALSE,
  col_prefix = "bt_",
  add_to_phyloseq = TRUE,
  cache_dir = tools::R_user_dir("taxinfo", "cache"),
  refresh = FALSE,
  verbose = TRUE
)
```

## Arguments

- physeq:

  (required) A phyloseq object with bacterial taxonomic ranks in its
  \`tax_table\`.

- file_name_species, file_name_genus:

  (Character, default \`NULL\`) Paths to the BactoTraits species-level
  and genus-level CSV files (\`BACTOTRAITS_DATASET\_\*\_SPECIESLVL.csv\`
  and \`\*\_GENUSLVL.csv\`). When \`NULL\` (and the corresponding levels
  are requested), the files are downloaded once from
  \<https://doi.org/10.24396/ORDAR-182\> and cached in \`cache_dir\`.
  Only the genus-level file is used for levels coarser than \`genus\`.

- taxonomic_rank:

  (Character vector of 6, default \`c("Phylum", "Class", "Order",
  "Family", "Genus", "Species")\`) The columns of the \`tax_table\` slot
  holding the six BactoTraits ranks, from the highest (first) to the
  most precise (last). Their names need not match the BactoTraits column
  names: they are matched positionally.

- level:

  (Character vector, default all six) Matching levels allowed. Their
  order does not matter: the most precise allowed level that matches
  always wins (\`species\` \> \`genus\` \> \`family\` \> \`order\` \>
  \`class\` \> \`phylum\`). Use e.g. \`c("species", "genus")\` to forbid
  coarser aggregation.

- merge_modalities:

  (Logical, default \`TRUE\`) If \`TRUE\`, every \*binary\* trait (two
  modalities, e.g. \`motility_yes\` / \`motility_no\`) is collapsed to a
  single column named after the trait (\`motility\`) with values
  \`TRUE\`, \`FALSE\` or a probability (see Details). Traits with more
  than two modalities (e.g. \`cell_shape\`) cannot be encoded in one
  value and keep their raw modality columns.

- positive_modalities:

  (Character vector, default \`c("yes", "positive", "present",
  "true")\`) Modality names that define the "positive" side of a binary
  trait. The first matching modality is the one encoded in the merged
  column; when none matches, the first modality in column order is used.

- raw_cols:

  (Logical, default \`FALSE\`) If \`TRUE\`, keep (in addition to the
  merged columns when \`merge_modalities = TRUE\`) every raw
  \`trait_modality\` column. Always \`TRUE\` when \`merge_modalities =
  FALSE\`.

- col_prefix:

  (Character, default \`"bt\_"\`) Prefix added to every new column of
  the \`tax_table\`.

- add_to_phyloseq:

  (Logical, default \`TRUE\`) If \`TRUE\`, return a new phyloseq object
  with new columns in the \`tax_table\` slot. If \`FALSE\`, return a
  tibble of the augmented \`tax_table\`.

- cache_dir:

  (Character) Directory used to cache the downloaded BactoTraits files.
  Defaults to \[tools::R_user_dir()\].

- refresh:

  (Logical, default \`FALSE\`) If \`TRUE\`, re-download the BactoTraits
  files even if they are already cached.

- verbose:

  (Logical, default \`TRUE\`) If \`TRUE\`, prompt some messages.

## Value

Either an updated phyloseq object (when \`add_to_phyloseq = TRUE\`) or a
tibble of the augmented \`tax_table\`, with the new columns
\`bt_trait_level\`, \`bt_n_strains\`, one merged column per binary trait
(\`bt_gram_stain\`, \`bt_motility\`, \`bt_pigmentation_production\`,
\`bt_forms_multicellular_complex\`, \`bt_spore_formation\`,
\`bt_energy_source\`, \`bt_electron_donnor\`, \`bt_carbon_source\`) and
the raw modality columns of the non-binary traits (e.g.
\`bt_cell_shape_coccus-shaped\`).

## Details

\*\*Merged binary columns.\*\* For a binary trait the reference modality
is the first one whose modality name is in \`positive_modalities\` (e.g.
\`yes\` for \`motility\`, \`positive\` for \`gram_stain\`), falling back
to the first modality in column order (\`chemotroph\` for
\`energy_source\`, \`autotroph\` for \`carbon_source\`, ...). The merged
column encodes the affinity \`a\` of the reference modality as \`TRUE\`
when \`a == 1\` (the taxon is assigned exclusively to that modality),
\`FALSE\` when \`a == 0\`, and the affinity score itself (a fuzzy-coded
probability in \`\]0, 1\[\`) otherwise. As everywhere in the
\`tax\_\*\_pq\` family, columns are character once they reach the
\`tax_table\` matrix.

\*\*Rank fallback.\*\* A taxon whose species is absent from the
species-level file falls back to its genus (genus-level file), then to
the strain-weighted mean profile of its family, order, class or phylum
(computed from the genus-level rows). The profile comes \*entirely\*
from the most precise assignable level: levels are never mixed
column-wise. Use \`trait_level\` to filter on the match level and
\`n_strains\` (the number of BacDive strains behind the profile) to
filter on data support.

\*\*Species names.\*\* The species-level key is the binomial
\`"\<Genus\> \<epithet\>"\`. The \`Species\` rank column may hold that
binomial, a bare epithet (glued to the genus), or the genus directly
followed by the epithet without a space, as in \`GlobalPatterns\`
(\`"Sulfolobusacidocaldarius"\` for genus \`"Sulfolobus"\`, read as
\`"Sulfolobus acidocaldarius"\`). Other values (e.g. clone names such as
\`"SCA1145"\`) find no species match and fall back to coarser levels.

## References

Laderriere, V., Usseglio-Polatera, F., Maunoury-Danger, F. & Cebron, A.
(2026). BactoTraits: a trait database for exploring functional diversity
of bacterial communities. \*Scientific Data\* 13, 337.
[doi:10.1038/s41597-026-06652-2](https://doi.org/10.1038/s41597-026-06652-2)

## See also

\[tax_metatraits_pq()\], \[tax_faprotax_pq()\], \[tax_info_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
# \donttest{
data(GlobalPatterns, package = "phyloseq")

# The bundled mini files keep the example fast and fully offline
res <- tax_bactotraits_pq(GlobalPatterns,
  file_name_species = system.file("extdata", "bactotraits_species_mini.csv",
    package = "taxinfo"
  ),
  file_name_genus = system.file("extdata", "bactotraits_genus_mini.csv",
    package = "taxinfo"
  ),
  verbose = FALSE
)
table(res@tax_table[, "bt_trait_level"], useNA = "ifany")
#> 
#>   class  family   genus   order species    <NA> 
#>    4876     483    1038    3667       7    9145 
table(res@tax_table[, "bt_motility"], useNA = "ifany")
#> 
#> 0.00490196078431373 0.00584795321637427  0.0107421669507396   0.010752688172043 
#>                  50                 224                  35                1594 
#>   0.378378378378378   0.439511021726232   0.535714285714286   0.551719919934762 
#>                3605                  18                 246                1563 
#>   0.574468085106383               0.754   0.802447552447552   0.850344665075995 
#>                  67                 186                 467                1674 
#>   0.872159090909091    0.93731778425656               FALSE                TRUE 
#>                  83                 252                   5                   1 
#>                <NA> 
#>                9146 
# }

if (FALSE) { # \dontrun{
# Full BactoTraits database: the species- and genus-level files are
# downloaded from https://doi.org/10.24396/ORDAR-182 once, then cached
res_full <- tax_bactotraits_pq(GlobalPatterns)

# Keep the raw modality columns of the binary traits as well
res_raw <- tax_bactotraits_pq(GlobalPatterns, raw_cols = TRUE)
} # }
```
