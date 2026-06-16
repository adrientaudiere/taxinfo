# Plot the taxa occurrence using gbif.range package

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

A wrapper of \[gbif.range::get_gbif()\] function to plot the range of
taxa using ggplot2. The function can take either a phyloseq object or a
vector of taxonomic names. If a phyloseq object is provided, the
taxonomic names are extracted from the specified taxonomic rank. The
occurrences are plotted either as points or using hexagonal binning. The
function can also filter the occurrences by country.

## Usage

``` r
plot_tax_gbif_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  interactive_plot = FALSE,
  zcol = c("year", "taxonomicStatus"),
  hexagons = FALSE,
  bins = 100,
  verbose = TRUE,
  countries = NULL,
  info_names = c("country", "country code", "acceptedScientificName", "ScientificName"),
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE,
  ...
)
```

## Arguments

- physeq:

  (optional) A phyloseq object

- taxnames:

  (optional) A character vector of taxonomic names.

- taxonomic_rank:

  (Character, default "currentCanonicalSimple") The column(s) present in
  the @tax_table slot of the phyloseq object. Can be a vector of two
  columns (e.g. c("Genus", "Species")).

- interactive_plot:

  (logical, default FALSE) If TRUE, an interactive map is created using
  the package mapview.

- zcol:

  (character vector, default c("year", "taxonomicStatus")) Only used if
  interactive_plot is TRUE. The column(s) of the occurrences to use for
  coloring the points in the interactive map. See ?mapview::mapview()
  for more details.

- hexagons:

  (logical, default FALSE) Only used if interactive_plot is FALSE. If
  TRUE, use hexagonal binning to plot the occurrences. If FALSE, plot
  the occurrences as points.

- bins:

  (Number of bins for hexagonal binning, default 100) Only used if
  hexagons is TRUE and interactive_plot is FALSE.

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- countries:

  A character vector of country names to filter the occurrences. If NULL
  (default), all countries are used (no filter).

- info_names:

  (Character vector) The information to retrieve from GBIF for each
  occurrence. See \[gbif.range::get_gbif()\] for more details.

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

A list of ggplot2 objects, one for each taxon.

## See also

\[gbif.range::get_gbif()\], \[range_bioreg_pq()\],
\[tax_check_occur_pq()\], \[tax_check_ecoregion()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)

p <- plot_tax_gbif_pq(
  subset_taxa_pq(
    data_fungi_mini_cleanNames,
    taxa_sums(data_fungi_mini) > 20000
  ),
  hexagons = TRUE,
  verbose = TRUE, bins = 50, occ_samp = 100, grain = 10000
)

p <- plot_tax_gbif_pq(taxnames = c("Xylobolus subpileatus", "Stereum subpileatus"))

p <- plot_tax_gbif_pq(taxnames = c("Stereum ostrea", "Mycena renati"))
requireNamespace("patchwork")
p[[1]] / p[[2]] & no_legend()


p <- plot_tax_gbif_pq(
  taxnames = c("Xylobolus subpileatus", "Stereum  subpileatus"),
  hexagons = TRUE, verbose = FALSE
)

p <- plot_tax_gbif_pq(
  taxnames = c("Xylobolus subpileatus", "Stereum subpileatus"),
  hexagons = TRUE, verbose = FALSE, countries = c("france", "spain")
)

p[[1]] + coord_fixed(ylim = c(30, 50), xlim = c(-5, 25)) + no_legend()

p <- plot_tax_gbif_pq(
  taxnames = c(
    "Ossicaulis lachnopus",
    "Antrodiella brasiliensis",
    "Stereum ostrea",
    "Xylobolus subpileatus"
  ),
  hexagons = TRUE,
  verbose = F, bins = 50, occ_samp = 100, grain = 10000
)

requireNamespace("patchwork")
(p[[1]] + p[[2]]) /
  (p[[3]] + p[[4]]) & no_legend()
} # }
```
