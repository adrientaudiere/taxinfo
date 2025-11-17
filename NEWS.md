# taxinfo 0.1.2 (Development version)

## Major Changes

- **Changed default behavior**: The `add_to_phyloseq` parameter now defaults to `TRUE` when a phyloseq object is provided, and `FALSE` when using the `taxnames` parameter. This makes the workflow more intuitive - when working with phyloseq objects, the enriched object is returned by default.

## New Features

- All main functions (`gna_verifier_pq()`, `tax_gbif_occur_pq()`, `tax_get_wk_info_pq()`, `tax_globi_pq()`, `tax_info_pq()`, `tax_iucn_code_pq()`, `tax_oa_pq()`, `tax_occur_check_pq()`, `tax_photos_pq()`) now support the `taxnames` parameter, allowing users to query information for specific taxonomic names without a phyloseq object. 

- Added comprehensive tests for `taxnames` parameter usage across all functions.

- Add functions `extract_spores_mycodb()` and `tax_spores_size_pq()` to retrieve spore size information from MycoDB. 

- Add params `year_col` and `authorship_col` to `gna_verifier_pq()` to output year of publication and authorship information for each taxa.

- Add function `intra_taxnames_dist()` to compute pairwise DNA distances among taxa with the same taxonomic names.

- Add function `cluster_sbc()` to (post)cluster taxa into SBC (Species bound cluster) defined as  "clusters that include all and only ESVs assigned to one species, the sequence similarity threshold can vary between these clusters" by Riley *et al.* 2025 (<https://doi.org/10.1186/s12915-025-02284-x>). Also add a new vignette to illustrate the use of `cluster_sbc()`.

- Print information when using `tax_info_pq()` with `add_to_phyloseq = TRUE` to inform users that the phyloseq object is being updated.

- Add an example in `tax_info_pq()` manual with the EPPO database to determine if pest species regulated in France are found in the example phyloseq object. 

- Change the result column `genus` into `genusEpithet` from the `gna_verifier_pq()` function to avoid confusion between "Genus" and "genus" columns and to debug the use of duckdb in `taxinfo_pq()`.


## Bug fix

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
