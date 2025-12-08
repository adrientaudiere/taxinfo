# Package index

## Data Verification & Quality Control

Functions for verifying, cleaning, and standardizing taxonomic names
using authoritative databases and verification services.

- [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md)
  : Verify (and fix) scientific names (Genus species) of a phyloseq
  object.

## Add data from external sources

Core functions to retrieve and integrate taxonomic information from
various external databases and APIs.

### Custom csv file

- [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md)
  : Get information from a custom csv file using taxonomic names present
  in a phyloseq object

### GBIF occurences data

- [`tax_gbif_occur_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_occur_pq.md)
  : Get number of occurrences for each taxa of a phyloseq object

### Globi interactions data

- [`tax_globi_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_globi_pq.md)
  : Get biotic interactions for taxa present in a phyloseq object using
  rglobi

### Open Alex scientific publications data

- [`tax_oa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_oa_pq.md)
  : Get scientific works about taxa present in a phyloseq object

### Wikipedia & Knowledge Base Integration

Functions to access Wikipedia, Wikidata, and other knowledge bases for
taxonomic information, page statistics, and content analysis.

- [`tax_get_wk_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_info_pq.md)
  : Retrieve information about taxa from wikipedia
- [`tax_get_wk_lang()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_lang.md)
  : Retrieve the wikipedia pages for a given Wikidata taxon identifier
- [`tax_get_wk_pages_info()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_pages_info.md)
  : Retrieve information about wikipedia pages for a given taxon id

### Other sources

- [`tax_iucn_code_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_iucn_code_pq.md)
  : Get iucn conservation status through gbif
- [`tax_spores_size_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_spores_size_pq.md)
  : Extract spore size from mycoDB
- [`extract_spores_mycodb()`](https://adrientaudiere.github.io/taxinfo/reference/extract_spores_mycodb.md)
  : Extract spore size from mycoDB for a single species

## Data checking using external informations

Functions for validating, checking, and cross-referencing taxonomic
names.

### Using taxa occurence in gbif

- [`tax_occur_check()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_check.md)
  : Taxa occurrences check within a radius using GBIF data
- [`tax_occur_check_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_check_pq.md)
  : Check for taxa occurrences within a radius around samples using GBIF
  data
- [`tax_occur_multi_check_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_multi_check_pq.md)
  : Check for taxa occurrences within a radius around multiple samples
  using GBIF data
- [`tax_check_ecoregion()`](https://adrientaudiere.github.io/taxinfo/reference/tax_check_ecoregion.md)
  : Check if a GPS point is within an ecoregion where the species is
  present

### Using sequences from NCBI

- [`tax_retroblast_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_retroblast_pq.md)
  : Verify taxonomic assignment using BLAST against NCBI nucleotide
  database

## Geographic Analysis & Distribution

Tools for analyzing geographic distributions, biogeographic ranges, and
creating distribution maps from occurrence data.

- [`range_bioreg_pq()`](https://adrientaudiere.github.io/taxinfo/reference/range_bioreg_pq.md)
  [`plot_range_bioreg_pq()`](https://adrientaudiere.github.io/taxinfo/reference/range_bioreg_pq.md)
  : Get and plot the range of taxa within a bioregion using gbif.range
  package
- [`plot_tax_gbif_pq()`](https://adrientaudiere.github.io/taxinfo/reference/plot_tax_gbif_pq.md)
  : Plot the taxa occurrence using gbif.range package
- [`calculate_bbox()`](https://adrientaudiere.github.io/taxinfo/reference/calculate_bbox.md)
  : Calculate Bounding Box Around a Point

## Media & Photography

Functions to retrieve and manage taxonomic images, photographs, and
other media content.

- [`tax_photos_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_photos_pq.md)
  : Find photos of taxa from GBIF or Wikitaxa

## Data Selection & Filtering

Utilities for selecting, filtering, and manipulating taxonomic data
within phyloseq objects.

- [`select_taxa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/select_taxa_pq.md)
  : Select taxa in a phyloseq object based on names in a given column of
  the tax_table
- [`taxonomic_rank_to_taxnames()`](https://adrientaudiere.github.io/taxinfo/reference/taxonomic_rank_to_taxnames.md)
  : Extract taxonomic names from a phyloseq object

## Analyses and Cluster using taxonomic names

Functions for analyzing genetic distances among taxa with the same
taxonomic names and clustering into species bound cluster (SBC).

- [`intra_taxnames_dist()`](https://adrientaudiere.github.io/taxinfo/reference/intra_taxnames_dist.md)
  : Compute intra-taxanames distances for each taxa names
- [`cluster_sbc()`](https://adrientaudiere.github.io/taxinfo/reference/cluster_sbc.md)
  : Create Species-Bound Clusters using SWARM algorithm

## Utility Functions

Helper functions for package management, data validation, and ggplot2
themes.

- [`check_package()`](https://adrientaudiere.github.io/taxinfo/reference/check_package.md)
  : Check package availability and propose installation instructions
- [`taxa_summary_text()`](https://adrientaudiere.github.io/taxinfo/reference/taxa_summary_text.md)
  : Text summary for a taxonomic rank
- [`idest_colors()`](https://adrientaudiere.github.io/taxinfo/reference/idest_colors.md)
  : IdEst colors for ggplot theme_idest
- [`idest_pal`](https://adrientaudiere.github.io/taxinfo/reference/idest_pal.md)
  : IdEst color palettes
- [`scale_color_idest_c()`](https://adrientaudiere.github.io/taxinfo/reference/scale_color_idest_c.md)
  : IdEst continuous color scales for ggplot2
- [`scale_color_idest_d()`](https://adrientaudiere.github.io/taxinfo/reference/scale_color_idest_d.md)
  : IdEst discrete color scales for ggplot2
- [`scale_fill_idest_c()`](https://adrientaudiere.github.io/taxinfo/reference/scale_fill_idest_c.md)
  : IdEst continuous fill scales for ggplot2
- [`scale_fill_idest_d()`](https://adrientaudiere.github.io/taxinfo/reference/scale_fill_idest_d.md)
  : IdEst discrete fill scales for ggplot2
- [`theme_idest()`](https://adrientaudiere.github.io/taxinfo/reference/theme_idest.md)
  : ggplot theme for IdEst

## Package Information

Package documentation and metadata.

- [`taxinfo-package`](https://adrientaudiere.github.io/taxinfo/reference/taxinfo-package.md)
  :

  `taxinfo` package
