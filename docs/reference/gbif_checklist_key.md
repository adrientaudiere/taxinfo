# Resolve a checklist spec to a GBIF checklistKey

Resolve a checklist spec to a GBIF checklistKey

## Usage

``` r
gbif_checklist_key(checklist = NULL)
```

## Arguments

- checklist:

  \`NULL\` (use the \`taxinfo.gbif_checklist\` option, falling back to
  \[gbif_ambient_checklist()\]), \`"colxr"\`, \`"backbone"\`, or a raw
  GBIF dataset UUID (passed through unchanged).

## Value

A GBIF checklistKey string.

## Author

Adrien Taudiere
