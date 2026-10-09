# Resolve names to GBIF taxa (EXACT or HIGHERRANK matches)

Runs \[gbif_backbone_checklist()\] and keeps the \`EXACT\` /
\`HIGHERRANK\` matches, one row per (\`usageKey\`, \`canonicalName\`,
\`verbatim_name\`). When no name matches, GBIF returns neither a
\`usageKey\` nor a \`canonicalName\` column; the result is then a
zero-row tibble with these three columns, so callers can rely on them,
and a warning alert is shown (callers then skip every GBIF occurrence
call and return their usual, empty output).

## Usage

``` r
gbif_match_taxa(taxnames, checklist = NULL)
```

## Arguments

- taxnames:

  (character) Names to match.

- checklist:

  Checklist spec, see \[gbif_checklist_key()\].

## Value

A tibble with columns \`usageKey\`, \`canonicalName\` and
\`verbatim_name\`.

## Author

Adrien Taudiere
