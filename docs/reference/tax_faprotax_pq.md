# Add FAPROTAX functional-group annotations to a phyloseq object

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Annotates the taxa of a phyloseq object with the ecological/metabolic
functional groups of the FAPROTAX database (Louca et al. 2016,
[doi:10.1126/science.aaf4507](https://doi.org/10.1126/science.aaf4507)
). FAPROTAX maps cultured prokaryotic taxa (mostly at the genus and
species level) to functions such as \`methanogenesis\`,
\`aerobic_ammonia_oxidation\`, \`sulfate_respiration\`, etc.

The bundled database file is parsed and every functional group is
matched against the taxonomic lineage of each taxon. A taxon is assigned
to a group when one of the group's \`\*level\*level\*\` patterns is
found, in order, along its lineage (matching is case-insensitive,
exactly as in FAPROTAX). The \`add_group:\` / \`remove_group:\` /
\`intersect_group:\` set operations used to build composite groups are
evaluated in file order.

By default a single summary column (\`faprotax_groups\`, a
\`;\`-separated list of assigned groups) and a count column
(\`faprotax_n_groups\`) are added to the \`tax_table\`. Set \`binary =
TRUE\` to additionally get one logical \`TRUE\`/\`FALSE\`/\`NA\` column
per functional group that was assigned to at least one taxon. Taxa that
matched no group (\`faprotax_groups\` is \`NA\`) receive \`NA\` (not
found in the database) rather than \`FALSE\` (a real "definitely not in
this group").

## Usage

``` r
tax_faprotax_pq(
  physeq,
  faprotax_file = system.file("extdata", "FAPROTAX.txt", package = "taxinfo"),
  tax_levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
  col_prefix = "faprotax_",
  binary = FALSE,
  valid_word_symbols = "-",
  add_to_phyloseq = TRUE,
  verbose = TRUE
)
```

## Arguments

- physeq:

  (required) A phyloseq object.

- faprotax_file:

  (Character) Path to the FAPROTAX database text file. Defaults to the
  version bundled with the package (\`system.file("extdata",
  "FAPROTAX.txt", package = "taxinfo")\`).

- tax_levels:

  (Character vector) Names of the \`tax_table\` columns, from the
  highest to the lowest rank, that make up the lineage matched against
  FAPROTAX. Defaults to the 7 standard ranks. Missing columns are
  silently skipped.

- col_prefix:

  (Character, default \`"faprotax\_"\`) Prefix applied to all columns
  added to the \`tax_table\`.

- binary:

  (Logical, default \`FALSE\`) If \`TRUE\`, add one logical
  \`TRUE\`/\`FALSE\`/\`NA\` column per functional group (prefixed with
  \`col_prefix\`) in addition to the summary columns. \`NA\` marks taxa
  that were not found in the FAPROTAX database (\`faprotax_groups\` is
  \`NA\`); \`FALSE\` marks taxa that were found but do not belong to
  that group.

- valid_word_symbols:

  (Character, default \`"-"\`) Non-alphanumeric characters that count as
  part of a word when matching pattern tokens against the lineage,
  matching the official FAPROTAX \`–valid_word_symbols\` option. Every
  other character (including \`\_\`) is a word boundary, so e.g.
  \`\*Methanobacterium\*\` matches the GTDB name \`Methanobacterium_B\`.

- add_to_phyloseq:

  (Logical, default \`TRUE\`) If \`TRUE\`, return an updated phyloseq
  object. If \`FALSE\`, return a tibble of the augmented \`tax_table\`.

- verbose:

  (Logical, default \`TRUE\`) If \`TRUE\`, print progress messages.

## Value

Either an updated phyloseq object (when \`add_to_phyloseq = TRUE\`) or a
tibble of the augmented \`tax_table\`.

## Details

FAPROTAX is a manually curated database built from cultured
representatives, with names following the NCBI/Bergey taxonomy. Coverage
is therefore highest for classically named taxa and can be very low for
environmental lineages known only from GTDB placeholder names (e.g.
\`JAJZYD01\`). This is expected behaviour, not a bug: unmatched taxa
simply receive \`NA\`.

The bundled \`FAPROTAX.txt\` is redistributed verbatim, including its
original copyright notice and BSD-style license (Copyright (c) 2019,
Stilianos Louca).

## References

Louca, S., Parfrey, L. W., & Doebeli, M. (2016). Decoupling function and
taxonomy in the global ocean microbiome. \*Science\*, 353(6305),
1272-1277.
[doi:10.1126/science.aaf4507](https://doi.org/10.1126/science.aaf4507)

## See also

\[tax_metatraits_pq()\], \[fungal_traits_guilds()\], \[tax_info_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
data(GlobalPatterns, package = "phyloseq")

res <- tax_faprotax_pq(GlobalPatterns, verbose = FALSE)

head(sort(table(res@tax_table[, "faprotax_groups"], useNA = "ifany"), decreasing = TRUE))
#> 
#>                                                               <NA> 
#>                                                              13202 
#>                        aerobic_chemoheterotrophy;chemoheterotrophy 
#>                                                               2029 
#>                                     fermentation;chemoheterotrophy 
#>                                                               1059 
#>                sulfate_respiration;respiration_of_sulfur_compounds 
#>                                                                320 
#>                                            intracellular_parasites 
#>                                                                224 
#> cyanobacteria;oxygenic_photoautotrophy;photoautotrophy;phototrophy 
#>                                                                214 

# \donttest{
# One 0/1 column per functional group, then count the nitrifying bacteria
res_bin <- tax_faprotax_pq(GlobalPatterns, binary = TRUE, verbose = FALSE)
sum(as.logical(res_bin@tax_table[, "faprotax_nitrification"]), na.rm = TRUE)
#> [1] 101

# Restrict matching to the genus / species level only (fewer hits)
res_gs <- tax_faprotax_pq(
  GlobalPatterns,
  tax_levels = c("Genus", "Species"),
  verbose = FALSE
)

# Return a tibble instead of a phyloseq object
tib <- tax_faprotax_pq(GlobalPatterns, add_to_phyloseq = FALSE, verbose = FALSE)
# }
```
