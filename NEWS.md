# taxinfo 0.1.2

## Bug Fixes

- `theme_idest()`: when `x_is_species_name = TRUE` or `y_is_species_name = TRUE` is set, a message now indicates which axis will receive italic labels. This helps users catch the common mistake of passing `x_is_species_name = TRUE` when species names are on the y-axis (e.g. horizontal bar charts with `aes(x = n, y = sp)`), which previously caused ggplot2 to silently misinterpret the continuous x-axis as discrete and break the chart.


## Major Changes

- **Changed default behavior**: The `add_to_phyloseq` parameter now defaults to `TRUE` when a phyloseq object is provided, and `FALSE` when using the `taxnames` parameter. This makes the workflow more intuitive - when working with phyloseq objects, the enriched object is returned by default.

## New Features

- `tax_photos_pq()` now works correctly with `gallery = TRUE` regardless of the `add_to_phyloseq` value: when `add_to_phyloseq = TRUE` the gallery is printed as a side-effect and the updated phyloseq object is returned invisibly. The `pixture` package dependency has been removed; the gallery is now built with `htmltools` (available on CRAN). Two new parameters `img_height` and `img_width` replace the previous `h`/`w` arguments passed via `...` to `pixture::pixgallery()`.

- Add `fungal_traits_guilds()` to enrich a phyloseq `tax_table` with guild and trait information from both the FungalTraits and FUNGuild databases in a single call. The function automatically calls [gna_verifier_pq()] when `currentCanonicalSimple` is absent, and optionally produces consensus columns (`cons_trophicMode`, `cons_trophicMode_agreement`) comparing the two sources.

- All main functions (`gna_verifier_pq()`, `tax_gbif_occur_pq()`, `tax_get_wk_info_pq()`, `tax_globi_pq()`, `tax_info_pq()`, `tax_iucn_code_pq()`, `tax_oa_pq()`, `tax_occur_check_pq()`, `tax_photos_pq()`) now support the `taxnames` parameter, allowing users to query information for specific taxonomic names without a phyloseq object. 

- Added comprehensive tests for `taxnames` parameter usage across all functions.

- Add functions `extract_spores_mycodb()` and `tax_spores_size_pq()` to retrieve spore size information from MycoDB. 

- Add params `year_col` and `authorship_col` to `gna_verifier_pq()` to output year of publication and authorship information for each taxa.

- Add function `intra_taxnames_dist()` to compute pairwise DNA distances among taxa with the same taxonomic names.

- Add function `cluster_sbc()` to (post)cluster taxa into SBC (Species bound cluster) defined as  "clusters that include all and only ESVs assigned to one species, the sequence similarity threshold can vary between these clusters" by Riley *et al.* 2025 (<https://doi.org/10.1186/s12915-025-02284-x>). Also add a new vignette to illustrate the use of `cluster_sbc()`.

- Print information when using `tax_info_pq()` with `add_to_phyloseq = TRUE` to inform users that the phyloseq object is being updated.

- Add an example in `tax_info_pq()` manual with the EPPO database to determine if pest species regulated in France are found in the example phyloseq object. 

- Change the result column `genus` into `genusEpithet` from the `gna_verifier_pq()` function to avoid confusion between "Genus" and "genus" columns and to debug the use of duckdb in `taxinfo_pq()`.

- `gna_verifier_pq()` gains a `species_only` parameter (default `TRUE`): when `TRUE`, `currentCanonicalSimple` is set to `NA` for uninomial matches (`matchedCardinality == 1`, i.e. genus or higher-rank names with no species epithet). `genusEpithet` is always populated regardless of this setting; `specificEpithet` is always `NA` for uninomials independently of this parameter.


## Bug fix

- `gna_verifier_pq()`: fixed verbose summary always reporting 0 accepted/synonym names when `add_to_phyloseq = FALSE` (was incorrectly reading dropped columns from `res_verifier_clean` instead of `res_verifier`). Fixed `matchedCardinality` threshold used for "uninomial" reporting (was `== 2` instead of `== 1`). Fixed `genusEpithet` and `specificEpithet` being absent from the return value when `add_to_phyloseq = FALSE` and `genus_species_canonical_col = TRUE` (the function was returning the raw GNA result instead of the cleaned tibble). Fixed potential many-to-many join when `add_to_phyloseq = TRUE` by deduplicating on `submittedName` after `select()` rather than before.

- Fixed issue in functions `tax_gbif_occur_pq()` and `range_bioreg_pq()` due to the loss of the column verbatim_index in `rgbif::name_backbone_checklist()` (commit [c74602b](https://github.com/ropensci/rgbif/commit/c74602b1c50e9c5d1a1fd6251dcad23595a94345)).


## Documentation

- Updated documentation for all functions to clarify the new default behavior of `add_to_phyloseq`.
- Added examples showing both phyloseq and taxnames usage patterns.
- Updated vignettes to demonstrate the dual-input capability (phyloseq objects vs. taxonomic name vectors).
- Updated README to highlight the flexible input options.

# taxinfo 0.1.1

- Add `list_keywords` and `n_citation` columns in the return of `tax_oa_pq()`.

# taxinfo 0.1.0

* Initial github submission.
