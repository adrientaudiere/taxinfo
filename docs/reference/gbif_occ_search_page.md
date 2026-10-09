# Default \`fetch_page\`: one \`occ_search\` page for a single taxon key

Default \`fetch_page\`: one \`occ_search\` page for a single taxon key

## Usage

``` r
gbif_occ_search_page(taxon_key, limit, start, time_to_sleep = 0.3)
```

## Arguments

- taxon_key:

  A GBIF usage key.

- limit:

  (numeric) Page size.

- start:

  (numeric) Offset of the first record of the page.

- time_to_sleep:

  (numeric) Seconds to sleep before the call (GBIF rate-limiting
  courtesy delay).

## Value

A tibble with \`decimalLongitude\`, \`decimalLatitude\` and, when
available, \`countryCode\`, \`year\` and \`gbifID\`.

## Author

Adrien Taudiere
