# Merge per-taxon information into the tax_table of a phyloseq object

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Internal helper shared by the \`tax\_\*\_pq\` family. It owns the
"merge-back" step common to every function that augments a phyloseq
object with externally-fetched, per-taxon information: build the join
key from the \`taxonomic_rank\` column(s), handle \`col_prefix\`
collisions, left-join the information tibble, and rebuild the
\`tax_table\` slot while preserving the original taxa order and names.
Each \`tax\_\*\_pq\` function only has to produce \`info_tbl\`; the
external fetch stays in the caller.

## Usage

``` r
augment_tax_table(
  physeq,
  info_tbl,
  taxonomic_rank,
  info_key = "taxa_name",
  col_prefix = NULL,
  default_prefix = NULL,
  keep_key = TRUE
)
```

## Arguments

- physeq:

  (required) A phyloseq object.

- info_tbl:

  (required) A tibble or data frame with one row per taxon, keyed by
  \`info_key\`. All other columns are added to the \`tax_table\`.

- taxonomic_rank:

  (character) The column(s) of \`physeq@tax_table\` whose pasted value
  (via \[taxnames_from_rank()\]) forms the join key.

- info_key:

  (character, default \`"taxa_name"\`) The column of \`info_tbl\`
  holding the submitted query name to join on.

- col_prefix:

  (character, default \`NULL\`) Prefix added to every new column. If
  \`NULL\` and a new column collides with an existing \`tax_table\`
  column, \`default_prefix\` is used (with a warning). If supplied
  explicitly and a collision remains, the function aborts.

- default_prefix:

  (character, default \`NULL\`) Fallback prefix used on a collision when
  \`col_prefix\` is \`NULL\`. When both are \`NULL\`, a collision is a
  hard error.

- keep_key:

  (logical, default \`TRUE\`) If \`TRUE\`, retain the join key in the
  result as a \`taxa_name\` column (overwriting any existing one); if
  \`FALSE\`, drop it.

## Value

A phyloseq object whose \`tax_table\` carries the new columns, with the
original taxa order and \`taxa_names()\` preserved.

## Details

\*\*Key invariant.\*\* \`info_tbl\` must be keyed (column \`info_key\`)
by the \*submitted query name\* – the value produced by
\[taxnames_from_rank()\] / \[taxonomic_rank_to_taxnames()\] that was
sent to the external database – and \*\*not\*\* by whatever name the
database returned. Keying on the query name is what makes the join
correct without re-verifying names: both sides live in the same
namespace by construction. This module is deliberately network-free and
does not call \[gna_verifier_pq()\]; name harmonisation belongs upstream
(run \[gna_verifier_pq()\] first and pass \`taxonomic_rank =
"currentCanonicalSimple"\`) or inside the caller's fetch.

The join is a \`left_join\` with \`relationship = "many-to-one"\`: every
taxon is kept (unmatched taxa get \`NA\`), and a duplicated key in
\`info_tbl\` is a hard error rather than a silent row multiplication.

## See also

\[taxnames_from_rank()\], \[taxonomic_rank_to_taxnames()\]

## Author

Adrien Taudiere
