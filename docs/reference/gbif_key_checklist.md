# Checklist a set of GBIF taxon keys belongs to

Legacy GBIF Backbone keys are numeric (\`5231190\`), COL XR keys are
alpha-numeric (\`"Q2M4"\`): the same rule rgbif 3.9.0 uses to route its
occurrence queries.

## Usage

``` r
gbif_key_checklist(keys)
```

## Arguments

- keys:

  GBIF taxon keys.

## Value

\`"backbone"\` or \`"colxr"\`.

## Author

Adrien Taudiere
