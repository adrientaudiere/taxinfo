# One transition of the ecoregion-saturation state machine

One transition of the ecoregion-saturation state machine

## Usage

``` r
ecoregion_saturation_step(
  state,
  batch_keys,
  n_batch_points,
  min_points,
  patience
)
```

## Arguments

- state:

  A list with elements \`n_points\` (numeric), \`seen\` (character
  vector of grouping keys already observed) and \`stale\` (number of
  consecutive batches that brought no new key).

- batch_keys:

  (character) Grouping keys (e.g. \`ECO_NAME\` values) of the new batch.
  \`NA\` and empty values are ignored.

- n_batch_points:

  (numeric) Number of points in the new batch.

- min_points:

  (numeric) Minimum number of points before the rule is allowed to fire.

- patience:

  (numeric) Number of consecutive key-less batches tolerated before the
  set is declared saturated.

## Value

The updated \`state\`, with a new logical element \`saturated\`.

## Author

Adrien Taudiere
