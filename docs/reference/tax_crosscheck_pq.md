# Cross-check taxonomic names using GBIF backbone and GNA Verifier

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Compares name-verification results from two independent sources:

- **GNA Verifier** (via \[taxize::gna_verifier()\]) with \`data_sources
  = 11\` (GBIF Backbone Taxonomy)

- **rgbif backbone** (via \[rgbif::name_backbone_checklist()\])

Because the two services use different matching algorithms and update
schedules, discrepancies highlight taxa that may need manual review. A
Venn-style summary shows the overlap in matched canonical names.

## Usage

``` r
tax_crosscheck_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = c("Genus", "Species"),
  data_sources = 11,
  plot = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- physeq:

  (optional) A phyloseq object. Either \`physeq\` or \`taxnames\` must
  be provided, but not both.

- taxnames:

  (optional) A character vector of taxonomic names.

- taxonomic_rank:

  Character vector. The column(s) in the \`@tax_table\` slot used to
  construct taxon names when \`physeq\` is provided. Default
  \`c("Genus", "Species")\`.

- data_sources:

  Integer or character vector passed to \[taxize::gna_verifier()\].
  Default \`11\` (GBIF Backbone Taxonomy). Use \`c(1, 11)\` to also
  include Catalogue of Life, for example.

- plot:

  (logical, default \`TRUE\`). If \`TRUE\` and ggVennDiagram is
  installed, a Venn diagram of the two sets of matched canonical names
  is included in the returned list.

- verbose:

  (logical, default \`TRUE\`). Print progress messages.

- ...:

  Additional arguments passed to \[gna_verifier_pq()\].

## Value

A list with the following elements:

- `gna_results`: tibble returned by \[gna_verifier_pq()\] (with
  \`add_to_phyloseq = FALSE\`).

- `backbone_results`: tibble returned by
  \[rgbif::name_backbone_checklist()\].

- `comparison`: data.frame with one row per submitted taxon, columns for
  the canonical name from each source, and a `status` column (`"match"`,
  `"mismatch"`, `"gna_only"`, `"backbone_only"`, or `"both_na"`).

- `summary`: named numeric vector with counts of each status category.

- `venn_plot`: (optional) a ggVennDiagram object comparing the two sets
  of matched canonical names.

## See also

\[gna_verifier_pq()\], \[rgbif::name_backbone_checklist()\]

## Author

Adrien Taudière

## Examples

``` r
if (FALSE) { # \dontrun{
# Cross-check a phyloseq object
res <- tax_crosscheck_pq(data_fungi)
res$summary
res$comparison |> filter(status == "mismatch")

res$venn_plot 

res_taxref <- tax_crosscheck_pq(data_fungi, data_sources = 12)

# Cross-check a vector of names
res2 <- tax_crosscheck_pq(taxnames = c(
  "Trametopsis brasiliensis",
  "Fake species Waller 2022",
  "Russula"
))
res2$summary
} # }
```
