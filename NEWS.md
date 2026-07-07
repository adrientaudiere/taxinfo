# taxinfo 0.3.0 (Development version)

## New features

* `tax_bioshifts_pq()` attaches, for each taxon of a phyloseq object, a summary of documented range-shift rates from the current BioShifts release (the original BioShifts merged with the CoRE database, Comte et al. 2024) via `BioShiftR::get_shifts()`, adding mean latitudinal (`bioshift_LAT_rate`) and elevational (`bioshift_ELE_rate`) shift rates plus a `bioshift_n_records` count. Taxa are matched on the `taxonomic_rank` column(s) of the `tax_table`; by default the BioShifts names are harmonised with `gna_verifier_pq()` and the join is made on `currentCanonicalSimple` (disable with `skip_name_verification = TRUE`). A pre-fetched table can be supplied via `shifts_data` to skip the network query.

* `tax_faprotax_pq()` annotates the taxa of a phyloseq object with the ecological/metabolic functional groups of the FAPROTAX database (Louca et al. 2016), matching every group's `*level*level*` patterns (including the `add_group` / `subtract_group` composites) against the taxonomic lineage; it adds a `faprotax_groups` summary column (and, with `binary = TRUE`, one 0/1 column per group). The FAPROTAX database is bundled in `inst/extdata/FAPROTAX.txt`. Its output was validated to be identical, per OTU and per functional group, to the official FAPROTAX Python tool (`collapse_table.py`) on `phyloseq::GlobalPatterns` (19216 taxa) and on a GTDB archaeal dataset (541 taxa). A `valid_word_symbols` argument (default `"-"`) reproduces FAPROTAX's word-boundary rules, so `_` acts as a boundary (e.g. `*Methanobacterium*` matches the GTDB name `Methanobacterium_B`).

* `tax_harmonize_backbone_pq()` re-derives the higher taxonomic ranks (those above a chosen anchor rank, or above each taxon's deepest assigned rank with `anchor = "last_assigned"`) from a single trusted backbone (GBIF online via `rgbif::name_backbone_checklist()` or a local `backbone` data frame), making taxonomies from different reference databases comparable; it harmonises several databases at once through suffix-based tracks (e.g. `suffixes = c("", "_Euk")`), gates online matches on `matchType`/`confidence`, disambiguates homonyms with `kingdom`, optionally recovers ambiguous names that GBIF backs off to a higher rank via the verbose `rgbif::name_backbone()` alternatives (`resolve_ambiguous = TRUE`, e.g. placing a bare `Boletus` in Boletaceae), and optionally keeps the overwritten values in `_orig` companion columns.

* `tax_metatraits_pq()` augments the `tax_table` with harmonised microbial phenotypic traits from the metaTraits resource (Robbani et al. 2026, <https://metatraits.embl.de>). It matches GTDB names given by the `taxonomic_rank` column(s) (species-first, with genus fallback), downloads and caches the large summary tables once in `tools::R_user_dir("taxinfo", "cache")`, and supports filtering by `traits`, `groups` (metaTraits `group_1` category) and `min_consensus_percentage`.

* `tax_spores_volume_pq()` annotates the taxa of a phyloseq object with fungal spore volume and morphology (length, width, projected area and length/width ratio) from the spore-trait database of Aguilar-Trigueros et al. (2023), redistributed by the `q2-fungal-traits` plugin and bundled in `inst/extdata/Spore_data_12Nov21.tsv`. For each spore type (`Mitospores`, `Meiospores`, `Multinucleate sexual spores`, `Multinucleate asexual spores`) it matches taxa hierarchically (species binomial, then genus, then family, with genus/family values the geometric mean of the database entries) and records the matched rank in a `*_matching_level` column, restricting matches to fungal taxa. Species-, genus- and family-level matches were cross-checked against a recomputation of the geometric-mean matching logic on the bundled database.

## Changes

* `tax_faprotax_pq()` now stores the per-group `binary = TRUE` columns as logical `TRUE`/`FALSE`/`NA` instead of integer `0`/`1`. Taxa that matched no FAPROTAX group (`faprotax_groups` is `NA`) receive `NA` (not found in the database) rather than `0`/`FALSE` (a real "definitely not in this group"); `faprotax_n_groups` is likewise `NA` for those taxa.

# taxinfo 0.2.0

## Bug fixes

* The phyloseq-augmenting functions `gna_verifier_pq()`, `tax_gbif_occur_pq()`, `tax_get_wk_info_pq()`, `tax_globi_pq()`, `tax_iucn_code_pq()`, `tax_oa_pq()`, `tax_occur_check_pq()` and `tax_photos_pq()` now build the `tax_table` join key with the same `"NA"`-token cleanup used to query the external database, fixing silently dropped information for genus-only taxa when `taxonomic_rank` spans several columns (e.g. `c("Genus", "Species")`).

* `tax_gbif_occur_pq()` and `tax_iucn_code_pq()` now join GBIF results to the `tax_table` on the submitted query name rather than GBIF's resolved `canonicalName`, so occurrence counts and IUCN codes are no longer silently dropped when GBIF maps a name to a different canonical (e.g. a synonym).

## Changes

* `gna_verifier_pq()` gains a `max_age_days` parameter (default `365`) and reports an informative message when a selected `data_sources` entry was last updated more than that many days ago; its `data_sources` documentation now explains how to adapt the source to the downstream database (e.g. `data_sources = 11`, the GBIF Backbone, before the GBIF-based functions).

* `tax_ecoregion_occur_pq()` now disambiguates colliding `tax_table` column names by prefixing them, consistent with the other `tax_*_pq()` functions, instead of overwriting the existing columns.

# taxinfo 0.1.2 [CRAN]
## Breaking changes

* The GBIF occurrence functions (`tax_gbif_occur_coords()`, `tax_occur_check()`, `tax_occur_check_pq()`, `tax_occur_multi_check_pq()`) now default to GBIF's Download API (`method = "download"`), which requires GBIF credentials (`GBIF_USER`, `GBIF_PWD`, `GBIF_EMAIL` in your `.Renviron`). The previous `rgbif::occ_search()` behaviour is still available with `method = "search"` (no credentials, capped at 100,000 records). See <https://docs.ropensci.org/rgbif/articles/gbif_credentials.html>.

* `tax_occur_check()` (and its `*_pq()` wrappers) now report the true worldwide georeferenced count in `total_count_in_world` for taxa with no occurrence in the search radius, where the previous behaviour returned `0`.

## Changes

* The WWF/TNC ecoregion layer downloaded on first use by the ecoregion functions (`points_to_ecoregions()`, `tax_check_ecoregion()`, `tax_ecoregion_occur()`) is now written to a stable per-user cache directory (`tools::R_user_dir("taxinfo", "cache")`) instead of an `inst/extdata/downloads` folder created under the current working directory.

* `gna_verifier_pq()` removes the `stats` and `main_taxon_threshold` parameters. These only affected kingdom-level summary metadata (not per-name results), and `main_taxon_threshold` was never forwarded to the API by `taxize::gna_verifier()` anyway.

* `gna_verifier_pq()` gains a `problematic_chars` parameter (default `"[?\\\\#|&]"`) to detect taxonomic names containing characters that corrupt the GNA Verifier GET URL (`?`, `\`, `#`, `|`, `&`), and a `clean_problematic_chars` parameter (default `FALSE`). When problematic names are found, a warning reports their count and examples; set `clean_problematic_chars = TRUE` to replace matching cells with `NA` before verification, or clean the data upstream (e.g. with `MiscMetabar::simplify_taxo()`).

* `gna_verifier_pq()` gains a `force_recompute` parameter (default `FALSE`). When `TRUE`, existing result columns matching `col_prefix` are removed from the `tax_table` before re-adding them, avoiding duplicate-column errors on re-runs.

* `select_taxa_pq()` aborts with an explicit message naming the requested `taxnames` when none of them match the `tax_table`, instead of failing with an obscure `OTU abundance data must have non-zero dimensions` error; `taxa_summary_text()` inherits the same clear behaviour.

* New function `tax_crosscheck_gbif_pq()` compares name-verification results from GNA Verifier (`taxize::gna_verifier()` with `data_sources = 11`, i.e. GBIF Backbone Taxonomy) and `rgbif::name_backbone_checklist()`. Returns a per-taxon comparison with status labels (`match`, `mismatch`, `gna_only`, `backbone_only`, `both_na`), a summary count vector, and an optional Venn diagram via `ggVennDiagram`. Discrepancies between the two services highlight taxa that may need manual review.

* `tax_ecoregion_occur()` gains a `method` argument (forwarded to `tax_gbif_occur_coords()`) and keeps the credential-free `rgbif::occ_search()` path as its default, so ecoregion profiling and its wrappers (`tax_ecoregion_occur_pq()`, `tax_check_ecoregion()`) do not require GBIF credentials.

* `tax_gbif_occur_coords()` gains a `method` argument (`"download"`, `"download_sql"`, `"search"`) and server-side filter arguments (`country`, `year_gte`, `year_lte`, `geometry`). The default `"download"` collapses the former per-taxon `rgbif::occ_search()` loop into a single, citable `rgbif::occ_download()` request and correctly retains infraspecific and higher-rank records.

* `tax_occur_check()` gains a `method` argument (`"download"`, `"search"`); with `method = "download"` it issues a single `rgbif::occ_download()` constrained to the search bounding box instead of `rgbif::occ_search()`.

* `tax_occur_check_pq()` and `tax_occur_multi_check_pq()` now issue a single GBIF download for all taxa (and, for `tax_occur_multi_check_pq()`, all GPS points) when `method = "download"`, instead of one `rgbif::occ_search()` call per taxon per point, and expose `method`, `circle_form`, `clean_coord` and `n_occur` arguments.

* `theme_idest()` falls back to the graphics-device default font when a requested font family (`Roboto Condensed`, `Linux Libertine G`, `Fira Code`) is not installed, instead of failing with `invalid font type` when the plot is printed (for example during `R CMD check` examples or a pkgdown render).

- `range_bioreg_pq()` and `tax_check_ecoregion()` now call `gbif.range::read_ecoreg()` and `gbif.range::check_and_get_ecoreg()` instead of the removed `read_bioreg()` / `check_and_get_bioreg()`.

## Bug Fixes

- `theme_idest()`: when `x_is_species_name = TRUE` or `y_is_species_name = TRUE` is set, a message now indicates which axis will receive italic labels. This helps users catch the common mistake of passing `x_is_species_name = TRUE` when species names are on the y-axis (e.g. horizontal bar charts with `aes(x = n, y = sp)`), which previously caused ggplot2 to silently misinterpret the continuous x-axis as discrete and break the chart.

## Breaking changes

- `tax_check_ecoregion()` no longer takes `taxa_name` as its first argument. The new signature follows the package-wide `physeq = NULL, taxnames = NULL, taxonomic_rank` pattern. Single-species positional calls like `tax_check_ecoregion("Sp.", lon, lat)` must become `tax_check_ecoregion(taxnames = "Sp.", longitudes = lon, latitudes = lat)`. The return shape also changes: `is_in_ecoregion` is always a `n_taxa × n_points` logical matrix, and the full long tibble of (taxon × ecoregion) counts is available in the new `taxon_ecoregions` element.

## Major Changes

- **Changed default behavior**: The `add_to_phyloseq` parameter now defaults to `TRUE` when a phyloseq object is provided, and `FALSE` when using the `taxnames` parameter. This makes the workflow more intuitive - when working with phyloseq objects, the enriched object is returned by default.

## New Features

- Add `points_to_ecoregions()` to locate a set of GPS points in the WWF/TNC terrestrial ecoregion layer. Returns a tibble with `ECO_NAME`, `biome` and `realm` columns; used internally by `tax_check_ecoregion()`.

- `tax_check_ecoregion()` has been rewritten as a thin comparison wrapper on top of the new `tax_ecoregion_occur()` and `points_to_ecoregions()` functions. It now supports a vector of taxa (via `taxnames`) or a phyloseq object (via `physeq` + `taxonomic_rank`), always returns a `n_taxa × n_points` logical matrix in `is_in_ecoregion`, and caches the WWF/TNC shapefile across calls instead of re-downloading it through `gbif.range::check_and_get_ecoreg()` each time. The shapefile is read via `sf::st_join()` (boundary-safe) instead of `sf::st_intersection()`.

- Add `tax_ecoregion_occur()` to return a long tibble of `taxon_name × ECO_NAME × n_occur × prop_occur` from GBIF occurrences, with `min_nb_occur` / `min_proportion` filters. Zero-occurrence taxa are kept with `n_occur = 0L` so downstream joins do not silently drop them.

- Add `tax_ecoregion_occur_pq()` as the phyloseq wrapper for `tax_ecoregion_occur()`. When `add_to_phyloseq = TRUE`, three columns are added to `@tax_table`: `ecoregion_top` (modal ecoregion), `ecoregion_n` (number of qualifying ecoregions) and `ecoregion_list` (semicolon-separated, ordered by descending occurrence count).

- Add `tax_gbif_occur_coords()` to fetch georeferenced GBIF occurrences for a vector of taxa (capped by `n_occur`). Taxa with zero valid occurrences are listed in `attr(result, "missing_taxa")`.

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

- `gna_verifier_pq()` now adds a `genusSpeciesEpithet` column (when `genus_species_canonical_col = TRUE`) that copies `currentCanonicalSimple` but is `NA` for genus-only names (i.e. when `specificEpithet` is `NA` or empty).
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
