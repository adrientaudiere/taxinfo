# Run a GBIF download and import the result

Internal helper that wraps the full asynchronous GBIF Download API
lifecycle (submit, wait, get, import, clean up) in a single call. It
accepts either a set of predicates (forwarded to
\[rgbif::occ_download()\]) or a SQL query (sent to
\[rgbif::occ_download_sql()\]). GBIF credentials are required; see
\[check_gbif_credentials()\].

## Usage

``` r
gbif_download(..., sql = NULL, format = "SIMPLE_CSV", verbose = TRUE)
```

## Arguments

- ...:

  Predicates built with \[rgbif::pred()\], \[rgbif::pred_in()\], etc.
  Passed to \[rgbif::occ_download()\]. Ignored when \`sql\` is supplied.

- sql:

  (character, default \`NULL\`). A SQL query string. When supplied, the
  download is submitted with \[rgbif::occ_download_sql()\] (server-side
  filtering and \`LIMIT\`) instead of predicates. The SQL Download API
  is a gated preview; the account must be enabled for it.

- format:

  (character, default \`"SIMPLE_CSV"\`). Download format passed to
  \[rgbif::occ_download()\]. Ignored when \`sql\` is supplied.

- verbose:

  (logical, default \`TRUE\`). If \`TRUE\`, print progress messages.

## Value

A tibble of imported occurrence records. The download key and DOI are
attached as \`attr(x, "key")\` and \`attr(x, "doi")\` for citation.

## See also

\[rgbif::occ_download()\], \[rgbif::occ_download_sql()\],
\[check_gbif_credentials()\]

## Author

Adrien Taudiere
