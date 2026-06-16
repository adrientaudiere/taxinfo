# Get biotic interactions for taxa present in a phyloseq object using rglobi

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

A wrapper of \[rglobi::get_interactions_by_taxa()\] function to get
biotic interactions for each taxa of a phyloseq object

## Usage

``` r
tax_globi_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  discard_synonym = TRUE,
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  interaction_types = NULL,
  valid_taxo_target_taxon = TRUE,
  add_target_canonical = TRUE,
  data_sources = c(1, 12),
  verbose = FALSE,
  strict_interaction_types = TRUE,
  max_interactions = 1000,
  batch_size_gna_verifier = 50,
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

- discard_synonym:

  (logical, default TRUE) If TRUE, discard interactions where the
  source_taxon_name is a synonym of the taxon name used to query

- add_to_phyloseq:

  (logical, default TRUE when physeq is provided, FALSE when taxnames is
  provided) If TRUE, return a new phyloseq object with new columns in
  the tax_table slot. If FALSE, return a tibble with the interactions
  found for each taxon. Automatically set to TRUE when a phyloseq object
  is provided and FALSE when taxnames is provided. Cannot be TRUE if
  \`taxnames\` is provided.

- col_prefix:

  A character string to be added as a prefix to the new columns names
  added to the tax_table slot of the phyloseq object (default: NULL).

- interaction_types:

  A character vector of interaction types to query. See
  \[rglobi::get_interaction_types()\]. If NULL (default), all
  interaction types are queried.

- valid_taxo_target_taxon:

  (logical, default TRUE) If TRUE, verify the scientific names of the
  target_taxon_name using \[taxize::gna_verifier()\] function and keep
  only valid names.

- add_target_canonical:

  (logical, default TRUE) If TRUE, add a column
  \`target_taxon_Canonical\` with the current accepted name (resolve the
  synonymie) of the target_taxon_name using \[taxize::gna_verifier()\]
  function.

- data_sources:

  A character or integer vector with numbers corresponding to data
  sources. See the Global Names Architecture documentation for a list of
  available options.

- verbose:

  (logical, default FALSE) If TRUE, prompt some messages.

- strict_interaction_types:

  (logical, default TRUE) If TRUE, keep only interactions exactly
  matching the interaction_types provided. If FALSE, keep all
  interactions returned by rglobi for the queried taxon. For exemple,
  rglobi for interaction_types = "hasHost" will also return interactions
  with interaction_type = "pathogenOf" and "parasiteOf" if
  strict_interaction_types is set to FALSE.

- max_interactions:

  (numeric, default 1000) The maximum number of interactions to query
  for each taxon.

- batch_size_gna_verifier:

  (numeric, default 100) The number of names to verify at once with'
  \[taxize::gna_verifier()\] function. Its a hack because gna_verifier
  seems to fail when too many names are sent at once including strange
  ones such as what is obtain whith rglobi. Only used if
  \`valid_taxo_target_taxon\` is set to TRUE.

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
\`rglobi\` and \`taxize\` packages.

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini,
  data_sources = 210
)

data_fungi_mini_cleanNames <- tax_globi_pq(data_fungi_mini_cleanNames,
  interaction_types = c("hasHost")
)

res_globi <- tax_globi_pq(data_fungi_mini,
  taxonomic_rank = c("Genus", "Species"),
  interaction_types = list("parasiteOf", "hasHost"),
  verbose = TRUE,
  max_interactions = 10
)
} # }
```
