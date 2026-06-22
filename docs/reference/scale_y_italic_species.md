# Discrete y-axis scale with species names in italic

A drop-in replacement for \[ggplot2::scale_y_discrete()\] that
automatically renders binomial species names (labels containing a space)
in italic using plotmath expressions, while leaving single-word labels
unchanged. Works with any theme, including complete themes like
\[theme_idest()\].

## Usage

``` r
scale_y_italic_species(...)
```

## Arguments

- ...:

  Arguments passed to \[ggplot2::scale_y_discrete()\].

## Value

A \`scale_y_discrete\` ggplot2 scale object.

## See also

\[scale_x_italic_species()\], \[label_italic_species()\]

## Author

Adrien Taudiere

## Examples

``` r
library(ggplot2)
df <- data.frame(
  sp = c("Russula nigricans", "Amanita", "Boletus edulis"),
  n  = c(10, 5, 8)
)
ggplot(df, aes(y = sp, x = n)) +
  geom_col() +
  theme_minimal() +
  scale_y_italic_species()
```
