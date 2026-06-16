# Get and plot the range of taxa within a bioregion using gbif.range package

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

A wrapper of \[gbif.range::get_gbif()\] and \[gbif.range::get_range()\]
functions to get and plot the range of taxa using ggplot2. The function
takes a phyloseq object as input and extracts the taxonomic names from
the specified taxonomic rank. The occurrences are plotted as points on a
map, along with the range of the taxon within a specified bioregion. The
bioregion used is "eco_terra", which corresponds to terrestrial
ecoregions defined by the World Wildlife Fund (WWF).

## Usage

``` r
range_bioreg_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  occ_samp = 5000,
  verbose = TRUE,
  verbose_gbif_range = FALSE,
  make_plot = FALSE,
  crop_plot = TRUE,
  remove_legend = TRUE,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE,
  ...
)

plot_range_bioreg_pq(...)
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

- occ_samp:

  (Numeric, default 5000) Number of occurrences to sample from GBIF. See
  \[gbif.range::get_gbif()\] for more details.

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- verbose_gbif_range:

  (logical, default TRUE) If TRUE, prompt some messages from gbif.range
  functions.

- make_plot:

  (logical, default TRUE) If TRUE, return a list of ggplot objects. Else
  return a list of range outputs from \[gbif.range::get_range()\].

- crop_plot:

  (logical, default TRUE) If TRUE, crop the plot to the extent of the
  bioregion.

- remove_legend:

  (logical, default TRUE) If TRUE, remove the legend from the plot.

- discard_genus_alone:

  (logical, default \`TRUE\` when \`taxonomic_rank ==
  "currentCanonicalSimple"\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- discard_NA:

  (logical, default \`TRUE\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- ...:

  Additional arguments to pass to \[gbif.range::get_gbif()\].

## Value

If make_plot = TRUE (default), a list of ggplot objects, one for each
taxon. If make_plot = FALSE, a list of range outputs from
\[gbif.range::get_range()\].

## Details

\[plot_range_bioreg_pq()\] is a wrapper of just a shortcut for
\`range_bioreg_pq(..., make_plot = TRUE)\`.

## See also

\[gbif.range::get_gbif()\], \[plot_tax_gbif_pq()\],
\[gbif.range::get_range()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{

data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_source = 210)

res_range <- range_bioreg_pq(subset_taxa(
  data_fungi_mini_cleanNames,
  currentCanonicalSimple %in% c("Xylodon flaviporus", "Basidiodendron eyrei")
), occ_samp = 100)

p <- plot_range_bioreg_pq(subset_taxa(
  data_fungi_mini_cleanNames,
  currentCanonicalSimple %in% c("Xylodon flaviporus", "Basidiodendron eyrei")
), occ_samp = 100)

requireNamespace("patchwork")
p[[1]] / p[[2]]

p <- plot_range_bioreg_pq(subset_taxa_pq(
  data_fungi_mini_cleanNames,
  taxa_sums(data_fungi_mini) > 20000
), occ_samp = 500)

p[[1]] / p[[2]]
} # }
```
