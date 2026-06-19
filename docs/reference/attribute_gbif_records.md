# Attribute downloaded GBIF records to the queried taxa

Internal helper that tags each record of a GBIF download with the
queried taxon it belongs to. A predicate download with
\`pred_in("taxonKey", keys)\` is \*hierarchical\*: it returns a taxon
and all its descendants, whose own \`taxonKey\` is more specific than
the queried key. A naive equality join on \`taxonKey\` therefore drops
infraspecific records (and every record of a higher-rank query). Records
are attributed by membership instead: a record belongs to queried key
\`K\` when its \`taxonKey\` \*or\* \`speciesKey\` equals \`K\`; as a
fallback for higher-rank matches, the queried \`canonicalName\` is
matched against the record's taxonomic name columns (\`species\`,
\`genus\`, \`family\`, …).

## Usage

``` r
attribute_gbif_records(occ_data, gbif_taxa)
```

## Arguments

- occ_data:

  (data frame) Imported GBIF occurrences (SIMPLE_CSV schema).

- gbif_taxa:

  (tibble) Resolved taxa with \`usageKey\`, \`canonicalName\` and
  \`verbatim_name\`.

## Value

\`occ_data\` with two added columns, \`taxon_name\` (the queried
\`verbatim_name\`) and \`usageKey\` (the queried key). Records may be
duplicated if they match more than one queried taxon.

## Author

Adrien Taudiere
