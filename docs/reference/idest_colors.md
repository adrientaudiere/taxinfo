# IdEst colors for ggplot theme_idest

IdEst colors for ggplot theme_idest

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
