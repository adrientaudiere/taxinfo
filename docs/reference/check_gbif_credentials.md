# Abort if GBIF credentials are missing

Internal helper that stops with an informative message (registration
link and \`.Renviron\` guidance) when the GBIF credentials required by
the Download API are not set. Used by \[gbif_download()\] and every
function that relies on \`rgbif::occ_download()\` /
\`rgbif::occ_download_sql()\`.

## Usage

``` r
check_gbif_credentials()
```

## Value

Invisibly \`TRUE\` when credentials are available; otherwise aborts.

## Author

Adrien Taudiere
