# Check that taxa were verified against the backbone queried downstream

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

\[gna_verifier_pq()\] records, for each taxon, the Global Names
Architecture (GNA) data source of its best match (columns
\`dataSourceId\` and \`dataSourceTitleShort\` of the \`tax_table\`).
This function compares these data sources with the backbone queried by a
downstream function and emits an informative message when they differ.
Mixing two sources of taxonomic truth (e.g. names verified against
TAXREF then queried in the GBIF backbone) silently lowers match rates,
because each backbone has its own synonymy and spelling.

By default the expected backbone is the GBIF checklist used by the GBIF
functions of taxinfo (see the \`taxinfo.gbif_checklist\` option in
\[gbif_key_to_col()\]): the GBIF backbone (GNA data source \`11\`) or
the Catalogue of Life Extended Release (closest GNA data source: \`1\`,
Catalogue of Life). The GBIF functions (\[tax_gbif_occur_pq()\],
\[tax_gbif_alt()\], \[tax_iucn_code_pq()\], \[tax_photos_pq()\],
\[tax_occur_check_pq()\], \[tax_ecoregion_occur_pq()\]) call this check
automatically when a phyloseq object is provided.

## Usage

``` r
check_backbone_pq(
  physeq,
  expected_source = NULL,
  checklist = NULL,
  taxonomic_rank = NULL,
  source_col = NULL,
  verbose = TRUE
)
```

## Arguments

- physeq:

  (phyloseq, required) A phyloseq object, typically the output of
  \[gna_verifier_pq()\].

- expected_source:

  (integer vector, default \`NULL\`) GNA data source id(s) of the
  backbone queried downstream (e.g. \`11\` for the GBIF backbone,
  \`210\` for TaxRef). If \`NULL\`, it is derived from \`checklist\`.

- checklist:

  (character, default \`NULL\`) GBIF checklist spec used when
  \`expected_source\` is \`NULL\`: \`"backbone"\`, \`"colxr"\` or
  \`NULL\` (use the \`taxinfo.gbif_checklist\` option). A custom
  checklist UUID cannot be mapped to a GNA data source, so the check is
  skipped.

- taxonomic_rank:

  (character, default \`NULL\`) The \`tax_table\` column(s) used as
  taxonomic names by the downstream function. If given, the check only
  runs when one of them is a \[gna_verifier_pq()\] output column (e.g.
  \`"currentCanonicalSimple"\`, possibly prefixed), and the data source
  column with the same prefix is used.

- source_col:

  (character, default \`NULL\`) Name of the data source column. If
  \`NULL\`, it is detected (column ending in \`"dataSourceId"\`).

- verbose:

  (logical, default \`TRUE\`) If \`TRUE\`, prompt the messages.

## Value

Invisibly, a tibble with one row per data source: \`dataSourceId\`,
\`dataSourceTitleShort\`, \`n_taxa\` and \`expected\` (logical, \`NA\`
for unmatched taxa), or \`NULL\` when no data source column is found.

## See also

\[gna_verifier_pq()\], \[gbif_key_to_col()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_taxref <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
# Names verified against TAXREF but queried in the GBIF backbone
check_backbone_pq(data_fungi_mini_taxref, checklist = "backbone")

# Expected backbone given explicitly
check_backbone_pq(data_fungi_mini_taxref, expected_source = 210)
} # }
```
