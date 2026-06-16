# Retrieve the wikipedia pages for a given Wikidata taxon identifier

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Filter only wikipedia page link to a language with a two- or
three-letter code defined by ISO 639-1 or ISO 639-3 (e.g. "en" for
English, "fr" for French, "de" for German). We also add a list of
more-than-three-letter codes for some languages: c("zh-yue", "nds-nl",
"ru-sib", "bat-smg", "fiu-vro", "roa-rup", "map-bms", "cbk-zam",
"roa-tara", "tokipona", "be-tarask", "zh-min-nan", "zh-classical"))

## Usage

``` r
tax_get_wk_lang(taxon_id, languages_pages = NULL)
```

## Arguments

- taxon_id:

  (Character string, required) The Wikidata taxon identifier (e.g.
  "Q10723171" for Xylobolus subpileatus)

- languages_pages:

  (Character vector) If not NULL, only the languages present in this
  vector will be queried.

## Value

A tibble with three columns: "title", "site" and "lang". NA values are
returned if wikipedia api return a response different from 200 or if the
taxon_id is set to NA or "". If no wikipedia page is found in the all
languages, a tibble with 0 is returned.

## See also

\[tax_get_wk_pages_info()\], \[tax_get_wk_info_pq()\],
\[tax_photo_pq()\]

## Examples

``` r
tax_get_wk_lang("Q10723171")
#> # A tibble: 5 × 3
#>   title                 site    lang 
#>   <chr>                 <chr>   <chr>
#> 1 Xylobolus subpileatus cebwiki ceb  
#> 2 Xylobolus subpileatus enwiki  en   
#> 3 Xylobolus subpileatus svwiki  sv   
#> 4 Xylobolus subpileatus szlwiki szl  
#> 5 Xylobolus subpileatus warwiki war  
tax_get_wk_lang("Q10723171") |>
  nrow()
#> [1] 5

tax_get_wk_lang("Q10723171")
#> # A tibble: 5 × 3
#>   title                 site    lang 
#>   <chr>                 <chr>   <chr>
#> 1 Xylobolus subpileatus cebwiki ceb  
#> 2 Xylobolus subpileatus enwiki  en   
#> 3 Xylobolus subpileatus svwiki  sv   
#> 4 Xylobolus subpileatus szlwiki szl  
#> 5 Xylobolus subpileatus warwiki war  
```
