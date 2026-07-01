# Build per-taxon names from taxonomic rank column(s)

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Internal key-builder shared by \[taxonomic_rank_to_taxnames()\] (the
query side) and \[augment_tax_table()\] (the merge side). It pastes the
\`taxonomic_rank\` column(s) of a \`tax_table\` into a single name per
taxon and applies the same \`"NA NA"\` / \`" NA"\` cleanup, so that the
names sent to an external database and the join key used to merge the
results back are guaranteed to be identical.

Unlike \[taxonomic_rank_to_taxnames()\], this helper never discards or
de-duplicates rows: it returns exactly one (possibly empty) string per
taxon, aligned to \`rownames(tax_table)\`.

## Usage

``` r
taxnames_from_rank(tax_table, taxonomic_rank, clean = TRUE)
```

## Arguments

- tax_table:

  A \`tax_table\` (taxonomyTable) or character matrix.

- taxonomic_rank:

  (character) The column(s) of \`tax_table\` to paste together, in order
  (e.g. \`"currentCanonicalSimple"\` or \`c("Genus", "Species")\`).

- clean:

  (logical, default \`TRUE\`) If \`TRUE\`, drop the \`"NA"\` tokens
  produced when a rank cell is missing (\`"Amanita NA"\` becomes
  \`"Amanita"\`, \`"NA NA"\` becomes \`""\`) and trim surrounding
  whitespace.

## Value

An unnamed character vector with one element per taxon.

## See also

\[taxonomic_rank_to_taxnames()\], \[augment_tax_table()\]

## Author

Adrien Taudiere
