# Resolve the phyloseq-or-taxnames input of a \`tax\_\*\_pq()\` function

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Internal helper shared by the \`tax\_\*\_pq\` family. It validates the
mutually exclusive \`physeq\` / \`taxnames\` input, resolves the default
for \`add_to_phyloseq\`, and extracts the taxon names from \`physeq\`
(via \[taxonomic_rank_to_taxnames()\]) when \`taxnames\` is not
supplied. It is the "front-matter" counterpart of the "merge-back"
helper \[augment_tax_table()\].

## Usage

``` r
resolve_taxa_input(
  physeq = NULL,
  taxnames = NULL,
  add_to_phyloseq = NULL,
  taxonomic_rank,
  discard_genus_alone = FALSE,
  discard_NA = TRUE
)
```

## Arguments

- physeq:

  (optional) A phyloseq object. Either \`physeq\` or \`taxnames\` must
  be provided, but not both.

- taxnames:

  (optional) A character vector of taxonomic names.

- add_to_phyloseq:

  (logical or \`NULL\`) The caller's \`add_to_phyloseq\` argument. When
  \`NULL\`, it defaults to \`TRUE\` if \`physeq\` is provided and
  \`FALSE\` otherwise. Aborts if \`TRUE\` while \`taxnames\` is
  supplied. Pass \`NA\` for callers that have no \`add_to_phyloseq\`
  concept (the returned value is then meaningless and can be ignored).

- taxonomic_rank:

  (character) The column(s) of \`physeq@tax_table\` used to build the
  taxon names. Forwarded to \[taxonomic_rank_to_taxnames()\].

- discard_genus_alone, discard_NA:

  (logical) Forwarded to \[taxonomic_rank_to_taxnames()\].

## Value

A list with two elements: \`taxnames\` (the resolved character vector)
and \`add_to_phyloseq\` (the resolved logical).

## See also

\[augment_tax_table()\], \[taxonomic_rank_to_taxnames()\]

## Author

Adrien Taudiere
