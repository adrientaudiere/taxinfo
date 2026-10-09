# \`checklistKey\` argument for a GBIF occurrence call

GBIF occurrence endpoints interpret \`taxonKey\` in the default
checklist of the installed rgbif (GBIF Backbone before 3.9.0, COL XR
from 3.9.0 on), so a key resolved in the other checklist silently
matches no occurrence. This helper returns the \`checklistKey\` matching
the checklist of \`keys\` (see \[gbif_key_checklist()\]) when it differs
from that default, and an empty list otherwise so that requests stay
identical to plain rgbif calls.

## Usage

``` r
gbif_occ_checklist_args(keys, fun = rgbif::occ_search)
```

## Arguments

- keys:

  GBIF taxon keys sent in the occurrence call.

- fun:

  The rgbif function receiving the argument (used to check that it
  accepts \`checklistKey\`).

## Value

An empty list or \`list(checklistKey = \<uuid\>)\`, to splice into the
arguments of \`fun\`.

## Author

Adrien Taudiere
