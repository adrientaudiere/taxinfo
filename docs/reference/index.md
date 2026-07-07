# Package index

## Data Verification & Quality Control

Functions for verifying, cleaning, and standardizing taxonomic names
using authoritative databases and verification services.

- [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md)
  : Verify (and fix) scientific names (Genus species) of a phyloseq
  object.
- [`tax_crosscheck_gbif_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_crosscheck_gbif_pq.md)
  : Cross-check taxonomic names using GBIF backbone and GNA Verifier
- [`tax_harmonize_backbone_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_harmonize_backbone_pq.md)
  : Harmonise higher taxonomic ranks from a trusted backbone

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
- [`tax_gbif_alt()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_alt.md)
  : Get altitude range statistics for each taxa from GBIF

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

### Fungal guilds and traits (FungalTraits + FUNGuild)

- [`fungal_traits_guilds()`](https://adrientaudiere.github.io/taxinfo/reference/fungal_traits_guilds.md)
  : Add FungalTraits and FUNGuild information to a phyloseq object

### Other sources

- [`tax_bioshifts_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_bioshifts_pq.md)
  : Add range-shift information from the BioShifts database to a
  phyloseq object
- [`tax_faprotax_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_faprotax_pq.md)
  : Add FAPROTAX functional-group annotations to a phyloseq object
- [`tax_metatraits_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_metatraits_pq.md)
  : Add metaTraits phenotypic traits to a phyloseq object
- [`tax_iucn_code_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_iucn_code_pq.md)
  : Get iucn conservation status through gbif
- [`tax_spores_size_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_spores_size_pq.md)
  : Extract spore size from mycoDB
- [`extract_spores_mycodb()`](https://adrientaudiere.github.io/taxinfo/reference/extract_spores_mycodb.md)
  : Extract spore size from mycoDB for a single species
- [`tax_spores_volume_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_spores_volume_pq.md)
  : Add fungal spore volume and morphology to a phyloseq object

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
  : Check whether GPS points fall in ecoregions occupied by a set of
  taxa
- [`tax_gbif_occur_coords()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_occur_coords.md)
  : Get GBIF occurrence coordinates for a vector of taxa
- [`tax_ecoregion_occur()`](https://adrientaudiere.github.io/taxinfo/reference/tax_ecoregion_occur.md)
  : Count GBIF occurrences of taxa in each WWF/TNC terrestrial ecoregion
- [`tax_ecoregion_occur_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_ecoregion_occur_pq.md)
  : Count GBIF occurrences per ecoregion for the taxa of a phyloseq
  object
- [`points_to_ecoregions()`](https://adrientaudiere.github.io/taxinfo/reference/points_to_ecoregions.md)
  : Map GPS points to WWF/TNC terrestrial ecoregions

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
  : IdEst continuous fill scales for ggplot2 \<a
  href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
  \<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
  alt="lifecycle-experimental"\>\</a\>
- [`scale_fill_idest_d()`](https://adrientaudiere.github.io/taxinfo/reference/scale_fill_idest_d.md)
  : IdEst discrete fill scales for ggplot2 \<a
  href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
  \<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
  alt="lifecycle-experimental"\>\</a\>
- [`theme_idest()`](https://adrientaudiere.github.io/taxinfo/reference/theme_idest.md)
  : ggplot theme for IdEst
- [`label_italic_species()`](https://adrientaudiere.github.io/taxinfo/reference/label_italic_species.md)
  : Format taxon labels with species names in italic
- [`scale_x_italic_species()`](https://adrientaudiere.github.io/taxinfo/reference/scale_x_italic_species.md)
  : Discrete x-axis scale with species names in italic
- [`scale_y_italic_species()`](https://adrientaudiere.github.io/taxinfo/reference/scale_y_italic_species.md)
  : Discrete y-axis scale with species names in italic

## Package Information

Package documentation and metadata.

- [`taxinfo-package`](https://adrientaudiere.github.io/taxinfo/reference/taxinfo-package.md)
  :

  `taxinfo` package
