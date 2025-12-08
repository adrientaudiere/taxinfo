# IdEst discrete fill scales for ggplot2

IdEst discrete fill scales for ggplot2

## Usage

``` r
scale_fill_idest_d(
  palette_name = "all_color_idest",
  direction = 1,
  override_order = FALSE,
  ...
)
```

## Arguments

- palette_name:

  The name of the palette to use. The available palette are
  c("all_color_idest", "ligth_color_idest", "dark_color_idest",
  "Picabia", "Picasso", "Levine2", "Rattner", "Sidhu", "Hokusai2",
  "Hokusai3"). See \[idest_pal\] for more details.

- direction:

  Direction of the palette. 1 for standard, -1 for reversed.

- override_order:

  Logical (default FALSE), whether to override the order of the palette.

- ...:

  Additional arguments passed to \[ggplot2::scale_color_gradientn()\].

## Value

A ggplot2 scale object.

## Author

Adrien Taudiere
