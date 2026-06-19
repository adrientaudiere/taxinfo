# Are GBIF credentials available?

Non-throwing predicate that returns \`TRUE\` when all three GBIF
credential environment variables (\`GBIF_USER\`, \`GBIF_PWD\`,
\`GBIF_EMAIL\`) are set to a non-empty value. Single source of truth
used by \[check_gbif_credentials()\] and by the test helper
\`skip_if_no_gbif_credentials()\`.

## Usage

``` r
has_gbif_credentials()
```

## Value

A logical scalar.

## Author

Adrien Taudiere
