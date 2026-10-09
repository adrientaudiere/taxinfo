# Violin plot of GBIF altitudes with the altitude of the samples

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

For each taxon, draw the distribution of the altitudes of its GBIF
occurrences as a violin, and the altitude of each sample in which the
taxon is present as a horizontal line. The standardized effect size
(SES) computed by \[tax_alt_ses_pq()\] is written above each violin, and
taxa are ordered by SES.

## Usage

``` r
plot_tax_alt_pq(
  physeq,
  sample_alt_col,
  occ_alt,
  taxonomic_rank = "currentCanonicalSimple",
  taxa_col = "canonicalName",
  elevation_col = "elevation",
  min_records = 10,
  order_by_ses = TRUE,
  show_ses = TRUE,
  sample_color = "#C0392B"
)
```

## Arguments

- physeq:

  (phyloseq, required) A phyloseq object with a \`sam_data\` slot.

- sample_alt_col:

  (character, required) Name of the numeric column of the \`sam_data\`
  slot giving the altitude of each sample (in meters).

- occ_alt:

  (data.frame, required) GBIF altitudes of the taxa, with one row per
  occurrence (as returned by \[tax_gbif_alt()\] with \`return_raw =
  TRUE\`), with columns \`taxa_col\` and \`elevation_col\`.

- taxonomic_rank:

  (character, default \`"currentCanonicalSimple"\`) The column(s) of the
  \`tax_table\` giving the taxonomic names, matched against the
  \`taxa_col\` column of \`occ_alt\`. Can be a vector of two columns
  (e.g. \`c("Genus", "Species")\`).

- taxa_col:

  (character, default \`"canonicalName"\`) Name of the column of
  \`occ_alt\` with the taxonomic names.

- elevation_col:

  (character, default \`"elevation"\`) Name of the column of \`occ_alt\`
  with the occurrence altitudes (long format only).

- min_records:

  (integer, default \`10\`) Minimum number of GBIF altitudes for a taxon
  to be plotted.

- order_by_ses:

  (logical, default \`TRUE\`) If TRUE, taxa are ordered by increasing
  SES, else alphabetically.

- show_ses:

  (logical, default \`TRUE\`) If TRUE, the SES is written above each
  violin.

- sample_color:

  (character, default \`"#C0392B"\`) Color of the lines depicting the
  altitude of the samples.

## Value

A ggplot2 object.

## See also

\[tax_alt_ses_pq()\], \[tax_gbif_alt()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini,
  data_sources = 11
)
# Requires GBIF credentials (see ?tax_gbif_alt)
occ_alt <- tax_gbif_alt(data_fungi_mini_cleanNames, return_raw = TRUE)

# data_fungi_mini has no altitude: add a fictive one
sample_data(data_fungi_mini_cleanNames)$altitude <-
  runif(nsamples(data_fungi_mini_cleanNames), 0, 1500)
plot_tax_alt_pq(data_fungi_mini_cleanNames, "altitude", occ_alt)
} # }
```
