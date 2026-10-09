# Re-express resolved GBIF taxa with GBIF Backbone keys

GBIF SQL downloads filter on the literal \`taxonkey\` / \`specieskey\`
occurrence columns, which always hold GBIF Backbone numeric keys. Taxa
resolved in another checklist (COL XR) are therefore re-matched by name
in the GBIF Backbone; taxa without a backbone match are dropped with a
warning.

## Usage

``` r
gbif_taxa_to_backbone(gbif_taxa, verbose = TRUE)
```

## Arguments

- gbif_taxa:

  (tibble) Resolved taxa with \`usageKey\` and \`verbatim_name\`.

- verbose:

  (logical) Report the re-matching and the dropped taxa.

## Value

\`gbif_taxa\` with GBIF Backbone \`usageKey\`s (unchanged when its keys
already are backbone keys).

## Author

Adrien Taudiere
