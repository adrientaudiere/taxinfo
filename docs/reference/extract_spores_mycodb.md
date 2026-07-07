# Extract spore size from mycoDB for a single species

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

## Usage

``` r
extract_spores_mycodb(species_name, verbose = TRUE)
```

## Arguments

- species_name:

  Character. Species name, e.g. "Amanita muscaria"

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

## Value

A character string with the spore size, e.g. "8-10 x 6-8 \u00b5m". If
the species is not found in mycoDB, returns "Not in mycoDB", if the
species is found but no spore size info is available, returns "No spore
size info in mycoDB".

## See also

\[tax_spores_size_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
extract_spores_mycodb("Amanita muscaria")
#> [1] "10 x 8 µm"
extract_spores_mycodb("Boletus edulis")
#> ! Multiple spore size entries found for Boletus edulis. Using the first one.
#> [1] "13-18 x 4-6 µm"
extract_spores_mycodb("Xylobolus subpileatus")
#> [1] "No spore size info in mycoDB"
extract_spores_mycodb("Nonexistent species")
#> [1] "No spore size info in mycoDB"
extract_spores_mycodb("Amanita")
#> [1] "No spore size info in mycoDB"
```
