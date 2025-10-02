# taxinfo 0.1.2 (Development version)

## Major Changes

- **Changed default behavior**: The `add_to_phyloseq` parameter now defaults to `TRUE` when a phyloseq object is provided, and `FALSE` when using the `taxnames` parameter. This makes the workflow more intuitive - when working with phyloseq objects, the enriched object is returned by default. (#6)

## New Features

- All main functions (`gna_verifier_pq()`, `tax_gbif_occur_pq()`, `tax_get_wk_info_pq()`, `tax_globi_pq()`, `tax_info_pq()`, `tax_iucn_code_pq()`, `tax_oa_pq()`, `tax_occur_check_pq()`, `tax_photos_pq()`) now support the `taxnames` parameter, allowing users to query information for specific taxonomic names without a phyloseq object. (#6)
- Added comprehensive tests for `taxnames` parameter usage across all functions.

## Documentation

- Updated documentation for all functions to clarify the new default behavior of `add_to_phyloseq`.
- Added examples showing both phyloseq and taxnames usage patterns.
- Updated vignettes to demonstrate the dual-input capability (phyloseq objects vs. taxonomic name vectors).
- Updated README to highlight the flexible input options.

# taxinfo 0.1.1

- Add list_keywords and n_citation columns in the return of [tax_oa_pq()].

# taxinfo 0.1.0

* Initial github submission.
