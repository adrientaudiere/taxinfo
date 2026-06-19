# Build the SQL query used by \[tax_gbif_occur_coords()\] download_sql method

Build the SQL query used by \[tax_gbif_occur_coords()\] download_sql
method

## Usage

``` r
build_gbif_coords_sql(keys, country = NULL, year_gte = NULL, year_lte = NULL)
```

## Arguments

- keys:

  (integer) GBIF usage keys.

- country:

  (character or NULL) ISO2 country code.

- year_gte, year_lte:

  (numeric or NULL) Inclusive year bounds.

## Value

A single SQL query string for \[rgbif::occ_download_sql()\].

## Author

Adrien Taudiere
