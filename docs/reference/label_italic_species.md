# Format taxon labels with species names in italic

Returns plotmath expressions that render binomial or trinomial species
names (labels containing a space) in italic, leaving single-word labels
(genus, family, etc.) unchanged. Intended as a \`labels\` formatter for
discrete ggplot2 scales, or as a standalone helper.

Uses plotmath \`italic()\` expressions instead of markdown, so no
\`ggtext::element_markdown()\` theme element is required. This makes the
function compatible with any ggplot2 theme, including complete themes
such as \[theme_idest()\].

## Usage

``` r
label_italic_species(x)
```

## Arguments

- x:

  A character vector of taxon labels.

## Value

A list of plotmath expressions (for species names) and plain character
strings (for non-species labels), suitable for use as \`labels\` in
\[ggplot2::scale_x_discrete()\] or \[ggplot2::scale_y_discrete()\].

## See also

\[scale_x_italic_species()\], \[scale_y_italic_species()\]

## Author

Adrien Taudiere

## Examples

``` r
label_italic_species(c("Russula nigricans", "Amanita", "Boletus edulis"))
#> [[1]]
#> italic("Russula nigricans")
#> 
#> [[2]]
#> [1] "Amanita"
#> 
#> [[3]]
#> italic("Boletus edulis")
#> 
```
