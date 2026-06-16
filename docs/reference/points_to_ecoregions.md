# Map GPS points to WWF/TNC terrestrial ecoregions

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Assigns each GPS point (pair of longitude/latitude in decimal degrees,
WGS84) to the WWF/TNC terrestrial ecoregion, biome and realm that
contains it.

## Usage

``` r
points_to_ecoregions(longitudes, latitudes, ecoregions = NULL)
```

## Arguments

- longitudes:

  (numeric vector). Longitudes of the points to locate, in decimal
  degrees in \`\[-180, 180\]\`.

- latitudes:

  (numeric vector). Latitudes of the points to locate, in decimal
  degrees in \`\[-90, 90\]\`. Must have the same length as
  \`longitudes\`.

- ecoregions:

  (optional \`sf\` object, default \`NULL\`). Ecoregion polygon layer to
  use. If \`NULL\`, the shipped WWF/TNC layer is loaded via
  \[load_ecoregions()\] (result is cached, so passing \`NULL\` is
  usually the right choice).

## Value

A tibble with one row per input point and the columns \`point_id\`
(integer), \`longitude\`, \`latitude\`, \`ECO_NAME\`, \`biome\`,
\`realm\`. Points falling outside any ecoregion (oceans, poles...) have
\`NA\` in the three ecoregion columns.

## See also

\[tax_check_ecoregion()\], \[tax_ecoregion_occur()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
points_to_ecoregions(
  longitudes = c(2.3522, 4.2, -70),
  latitudes  = c(48.8566, 33, -33)
)
} # }
```
