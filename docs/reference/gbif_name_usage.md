# Backbone-safe \[rgbif::name_usage()\]

\`rgbif::name_usage()\` only understands legacy GBIF Backbone numeric
keys (it is deprecated for COL XR keys in rgbif 3.9.0 and cannot resolve
them, even with an explicit \`datasetKey\`). This wrapper pins the
backbone \`datasetKey\` and rejects alpha-numeric keys with an
actionable error. Call sites using it (\[tax_iucn_code_pq()\],
\[tax_photos_pq()\]) therefore resolve names with \`checklist =
"backbone"\`.

## Usage

``` r
gbif_name_usage(key, data = "all", ...)
```

## Arguments

- key:

  A GBIF Backbone numeric usage key.

- data:

  (character) Passed to \[rgbif::name_usage()\].

- ...:

  Passed on to \[rgbif::name_usage()\].

## Author

Adrien Taudiere
