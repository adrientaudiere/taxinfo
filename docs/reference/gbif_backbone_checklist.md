# Checklist-pinned \[rgbif::name_backbone_checklist()\]

Same interface as \[rgbif::name_backbone_checklist()\], but the target
checklist can be pinned explicitly (see \[gbif_key_to_col()\]) so that
resolved keys do not depend on the installed rgbif version. The
\`checklistKey\` parameter is only sent when it differs from the
checklist the installed rgbif uses by default: requests stay identical
to plain rgbif calls unless a pin is actually required. All taxinfo call
sites resolve GBIF names through this wrapper.

## Usage

``` r
gbif_backbone_checklist(name_data, checklist = NULL, ...)
```

## Arguments

- name_data:

  (character vector or data frame) Names to match.

- checklist:

  Checklist spec, see \[gbif_checklist_key()\].

- ...:

  Passed on to \[rgbif::name_backbone_checklist()\].

## Author

Adrien Taudiere
