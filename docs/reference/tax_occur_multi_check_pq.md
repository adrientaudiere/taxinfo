# Check for taxa occurrences within a radius around multiple samples using GBIF data

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

This function performs a species range check for taxa contained in a
phyloseq object, for multiple samples based on their geographic
coordinates (longitude and latitude).

## Usage

``` r
tax_occur_multi_check_pq(
  physeq = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  min_occur = 0,
  verbose = TRUE,
  lon_column = NULL,
  longitudes = NULL,
  lat_column = NULL,
  latitudes = NULL,
  radius_km = 50,
  n_occur = 1000,
  method = c("download", "search"),
  circle_form = TRUE,
  clean_coord = TRUE,
  clean_coord_verbose = FALSE,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE,
  ...
)
```

## Arguments

- physeq:

  (required) A phyloseq object.

- taxonomic_rank:

  The taxonomic rank to use for the check. Default is
  "currentCanonicalSimple" which corresponds to the cleaned scientific
  names in the phyloseq object if \[gna_verifier_pq()\] was used with
  default parameter.

- min_occur:

  Minimum number of occurrences in the radius to keep the taxon
  (default: 0).

- verbose:

  (Logical, default: TRUE). Whether to print progress messages.

- lon_column:

  Column name in sample_data containing longitudes.

- longitudes:

  Vector of longitudes corresponding to samples in the phyloseq object.
  If provided, it overrides lon_column.

- lat_column:

  Column name in sample_data containing latitudes.

- latitudes:

  Vector of latitudes corresponding to samples in the phyloseq object.
  If provided, it overrides lat_column.

- radius_km:

  Numeric. Search radius in kilometers (default: 50). See
  ?\[tax_occur_check_pq()\].

- n_occur:

  Numeric (default: 1000). Maximum number of occurrences to retrieve
  from GBIF for each taxon.

- method:

  (character, default \`"download"\`). How occurrences are fetched.
  \`"download"\` issues a \*\*single\*\* \[rgbif::occ_download()\]
  covering all taxa over the bounding box of every GPS point
  (\*\*requires GBIF credentials\*\*); \`"search"\` uses a per-taxon
  \[rgbif::occ_search()\] loop. See \[tax_occur_check()\].

- circle_form:

  (Logical, default: TRUE). Whether to use a circular search area. If
  FALSE, a square bounding box is used.

- clean_coord:

  (Logical, default: TRUE). Whether to clean coordinates using
  \`CoordinateCleaner\`.

- clean_coord_verbose:

  (Logical, default: FALSE). Whether to print messages from
  \`CoordinateCleaner\`.

- discard_genus_alone:

  (logical, default \`TRUE\` when \`taxonomic_rank ==
  "currentCanonicalSimple"\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- discard_NA:

  (logical, default \`TRUE\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- ...:

  Additional parameters (currently unused; reserved for forward
  compatibility).

## Value

A list containing: - A tibble resulting from the concatenation of result
of function \[tax_occur_check()\] for each GPS position. - A matrix of
samples x taxa with the number of occurrences in the radius for each
case of the matrix. - A new phyloseq object with taxa filtered based on
min_occur. Be careful, the filtering may be very stringent.

## See also

\[tax_occur_check()\], \[tax_occur_multi_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_cleanNames <-
  gna_verifier_pq(data_fungi_mini,
    data_sources = 210
  )
res_occur_check <-
  tax_occur_multi_check_pq(subset_samples(data_fungi_mini_cleanNames, Diameter == 52),
    longitudes = c(8.31, 8.31, 8.64, -1.19, 7.03),
    latitudes = c(47.38, 47.38, 45.83, 43.65, 43.93)
  )
} # }
```
