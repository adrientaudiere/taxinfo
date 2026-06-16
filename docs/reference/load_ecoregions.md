# Load WWF/TNC terrestrial ecoregions as an \`sf\` object

Internal helper that returns the terrestrial ecoregions polygon layer
used by \[tax_ecoregion_occur()\], \[tax_check_ecoregion()\] and
\[points_to_ecoregions()\]. The layer is read from the shapefile shipped
with the package
(\`inst/extdata/downloads/eco_terra/tnc_terr_ecoregions.shp\`) and
cached in a package-internal environment so that repeated calls are
free.

## Usage

``` r
load_ecoregions(ecoreg_name = "eco_terra", refresh = FALSE)
```

## Arguments

- ecoreg_name:

  (character, default \`"eco_terra"\`). Currently only \`"eco_terra"\`
  is supported; the argument is kept for future extension.

- refresh:

  (logical, default \`FALSE\`). If \`TRUE\`, force a re-read from disk
  and refresh the cache.

## Value

An \`sf\` object with valid geometries and at least the columns
\`ECO_NAME\`, \`BIOME\` (or \`WWF_MHTNAM\`) and \`REALM\` (or
\`WWF_REALM2\`).

## Author

Adrien Taudiere
