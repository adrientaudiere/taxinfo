# IdEst colors for ggplot theme_idest

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

## Usage

``` r
idest_colors(
  palette_name = "all_color_idest",
  n,
  type = c("discrete", "continuous"),
  direction = c(1, -1),
  override_order = FALSE
)
```

## Arguments

- palette_name:

  The name of the palette to use. The available palette are
  c("all_color_idest", "ligth_color_idest", "dark_color_idest",
  "Picabia", "Picasso", "Levine2", "Rattner", "Sidhu", "Hokusai2",
  "Hokusai3"). See \[idest_pal\] for more details.

- n:

  Number of colors to return.

- type:

  Type of palette. Either "discrete" or "continuous".

- direction:

  Direction of the palette. 1 for standard, -1 for reversed.

- override_order:

  Logical, whether to override the order of the palette.

## Value

A vector of colors.

## Author

Adrien Taudiere

## Examples

``` r
idest_colors("Picasso", n = 3)
#> Warning: `idest_colors()` was deprecated in taxinfo 0.2.0.
#> ℹ Please use `ggplotpq::idest_colors()` instead.
#> [1] "#c2676d" "#995041" "#45939c"
#> attr(,"class")
#> [1] "palette"
#> attr(,"name")
#> [1] "Picasso"
barplot(rep(1, 6), col = idest_colors("Hokusai2"), border = NA)
```
