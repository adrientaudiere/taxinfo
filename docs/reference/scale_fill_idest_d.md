# IdEst discrete fill scales for ggplot2 \<a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\> \<img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"\>\</a\>

IdEst discrete fill scales for ggplot2 \<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

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

## Examples

``` r
library(ggplot2)
ggplot(mpg, aes(class, fill = drv)) +
  geom_bar() +
  scale_fill_idest_d()
#> Warning: `scale_fill_idest_d()` was deprecated in taxinfo 0.2.0.
#> ℹ Please use `ggplotpq::scale_fill_idest_d()` instead.
```
