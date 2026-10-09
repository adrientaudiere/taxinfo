# GBIF taxon keys as character strings

\`as.character()\` writes round numeric keys in scientific notation
(\`as.character(2e6)\` is \`"2e+06"\`), so numeric and character keys
would not compare equal. Numeric keys are written in full instead.

## Usage

``` r
gbif_key_chr(keys)
```

## Arguments

- keys:

  GBIF taxon keys (numeric or character).

## Value

A character vector (\`NA\` for \`NA\` keys).

## Author

Adrien Taudiere
