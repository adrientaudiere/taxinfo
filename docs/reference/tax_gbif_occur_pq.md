# Get number of occurrences for each taxa of a phyloseq object

A wrapper of \[rgbif::occ_search()\] function to get the number of
occurences. Optionally, the number of occurrences can be obtained by
years or by country.

## Usage

``` r
tax_gbif_occur_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  by_country = FALSE,
  by_years = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3
)
```

## Arguments

- physeq:

  (optional) A phyloseq object. Either \`physeq\` or \`taxnames\` must
  be provided, but not both.

- taxnames:

  (optional) A character vector of taxonomic names.

- taxonomic_rank:

  (Character, default "currentCanonicalSimple") The column(s) present in
  the @tax_table slot of the phyloseq object. Can be a vector of two
  columns (e.g. c("Genus", "Species")).

- add_to_phyloseq:

  (logical, default TRUE when physeq is provided, FALSE when taxnames is
  provided) If TRUE, add new column(s) in the tax_table of the phyloseq
  object. Automatically set to TRUE when a phyloseq object is provided
  and FALSE when taxnames is provided. Cannot be TRUE if \`taxnames\` is
  provided.

- col_prefix:

  A character string to be added as a prefix to the new columns names
  added to the tax_table slot of the phyloseq object (default: NULL).

- by_country:

  (logical, default FALSE) If TRUE, the number of occurences is computed
  by country

- by_years:

  (logical, default FALSE) If TRUE, the number of occurences is computed
  by years

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- time_to_sleep:

  (numeric, default 0.3) Time to sleep between two calls to
  rgbif::occ_search(). Useful to avoid to be blocked by GBIF. Try to
  increase this value if you are blocked by the error "To download GBIF
  occurrence data in bulk, please request..."

## Value

Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq object,
if add_to_phyloseq = TRUE, with new column(s) in the tax_table.

## Details

This function is mainly a wrapper of the work of others. Please cite
\`rgbif\` package.

## See also

\[rgbif::occ_search()\], \[plot_tax_gbif_pq()\], \[tax_occurr_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
data_fungi_mini_cleanNames <-
  gna_verifier_pq(data_fungi_mini)
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 45
#> • Taxa submitted for verification: 37
#> • Genus-level only taxa: 2
#> • Total matches found: 25
#> • Synonyms: 2 (including 2 at genus level)
#> • Accepted names: 23 (including 21 at genus level)

# \donttest{
data_fungi_mini_cleanNames <- tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_country = TRUE)
#> ■■                                 4% | ETA:  7s
#> ℹ Processing GBIF occurrences for Stereum ostrea
#> ■■                                 4% | ETA:  7s

#> ■■■■                               9% | ETA: 15s
#> ℹ Processing GBIF occurrences for Xylodon raduloides
#> ■■■■                               9% | ETA: 15s

#> ℹ Processing GBIF occurrences for Stereum hirsutum
#> ■■■■                               9% | ETA: 15s

#> ℹ Processing GBIF occurrences for Trametopsis brasiliensis
#> ■■■■                               9% | ETA: 15s

#> ℹ Processing GBIF occurrences for Basidiodendron eyrei
#> ■■■■                               9% | ETA: 15s

#> ℹ Processing GBIF occurrences for Sistotrema oblongisporum
#> ■■■■                               9% | ETA: 15s

#> ℹ Processing GBIF occurrences for Fomes fomentarius
#> ■■■■                               9% | ETA: 15s

#> ■■■■■■■■■■■                       35% | ETA:  8s
#> ℹ Processing GBIF occurrences for Mycena renati
#> ■■■■■■■■■■■                       35% | ETA:  8s

#> ℹ Processing GBIF occurrences for Helicogloea pellucida
#> ■■■■■■■■■■■                       35% | ETA:  8s

#> ℹ Processing GBIF occurrences for Radulomyces molaris
#> ■■■■■■■■■■■                       35% | ETA:  8s

#> ℹ Processing GBIF occurrences for Elmerina caryae
#> ■■■■■■■■■■■                       35% | ETA:  8s

#> ℹ Processing GBIF occurrences for Phanerochaete livescens
#> ■■■■■■■■■■■                       35% | ETA:  8s

#> ℹ Processing GBIF occurrences for Gloeohypochnicium analogum
#> ■■■■■■■■■■■                       35% | ETA:  8s

#> ℹ Processing GBIF occurrences for Hyphoderma roseocremeum
#> ■■■■■■■■■■■                       35% | ETA:  8s

#> ■■■■■■■■■■■■■■■■■■■■■             65% | ETA:  4s
#> ℹ Processing GBIF occurrences for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■■■■             65% | ETA:  4s

#> ℹ Processing GBIF occurrences for Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■■■             65% | ETA:  4s

#> ℹ Processing GBIF occurrences for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■■■             65% | ETA:  4s

#> ℹ Processing GBIF occurrences for Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■             65% | ETA:  4s

#> ℹ Processing GBIF occurrences for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■             65% | ETA:  4s

#> ℹ Processing GBIF occurrences for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■             65% | ETA:  4s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  1s
#> ℹ Processing GBIF occurrences for Laetisaria buckii
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  1s

#> ℹ Processing GBIF occurrences for Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  1s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Processing GBIF occurrences for Xylodon flaviporus

# Get data without adding to phyloseq
tax_gbif_occur_pq(data_fungi_mini_cleanNames, add_to_phyloseq = FALSE)
#> ℹ Processing GBIF occurrences for Stereum ostrea
#> ℹ Processing GBIF occurrences for Xylodon raduloides
#> ℹ Processing GBIF occurrences for Stereum hirsutum
#> ■■■■■■                            17% | ETA:  8s
#> ℹ Processing GBIF occurrences for Trametopsis brasiliensis
#> ■■■■■■                            17% | ETA:  8s

#> ℹ Processing GBIF occurrences for Basidiodendron eyrei
#> ■■■■■■                            17% | ETA:  8s

#> ℹ Processing GBIF occurrences for Sistotrema oblongisporum
#> ■■■■■■                            17% | ETA:  8s

#> ℹ Processing GBIF occurrences for Fomes fomentarius
#> ■■■■■■                            17% | ETA:  8s

#> ℹ Processing GBIF occurrences for Mycena renati
#> ■■■■■■                            17% | ETA:  8s

#> ℹ Processing GBIF occurrences for Helicogloea pellucida
#> ■■■■■■                            17% | ETA:  8s

#> ■■■■■■■■■■■■■■                    43% | ETA:  6s
#> ℹ Processing GBIF occurrences for Radulomyces molaris
#> ■■■■■■■■■■■■■■                    43% | ETA:  6s

#> ℹ Processing GBIF occurrences for Elmerina caryae
#> ■■■■■■■■■■■■■■                    43% | ETA:  6s

#> ℹ Processing GBIF occurrences for Phanerochaete livescens
#> ■■■■■■■■■■■■■■                    43% | ETA:  6s

#> ℹ Processing GBIF occurrences for Gloeohypochnicium analogum
#> ■■■■■■■■■■■■■■                    43% | ETA:  6s

#> ℹ Processing GBIF occurrences for Hyphoderma roseocremeum
#> ■■■■■■■■■■■■■■                    43% | ETA:  6s

#> ℹ Processing GBIF occurrences for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■                    43% | ETA:  6s

#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s
#> ℹ Processing GBIF occurrences for Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s

#> ℹ Processing GBIF occurrences for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s

#> ℹ Processing GBIF occurrences for Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s

#> ℹ Processing GBIF occurrences for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s

#> ℹ Processing GBIF occurrences for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s

#> ℹ Processing GBIF occurrences for Laetisaria buckii
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s

#> ℹ Processing GBIF occurrences for Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Processing GBIF occurrences for Xylodon flaviporus
#> # A tibble: 23 × 2
#>    Global_occurences canonicalName           
#>                <int> <chr>                   
#>  1             10259 Stereum ostrea          
#>  2              3255 Xylodon raduloides      
#>  3             92035 Stereum hirsutum        
#>  4                 7 Trametopsis brasiliensis
#>  5              1073 Basidiodendron eyrei    
#>  6              3051 Sistotrema oblongisporum
#>  7            150607 Fomes fomentarius       
#>  8              5441 Mycena renati           
#>  9                50 Helicogloea pellucida   
#> 10              3259 Radulomyces molaris     
#> # ℹ 13 more rows
tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_years = TRUE, add_to_phyloseq = FALSE)
#> ℹ Processing GBIF occurrences for Stereum ostrea
#> ℹ Processing GBIF occurrences for Xylodon raduloides
#> ℹ Processing GBIF occurrences for Stereum hirsutum
#> ℹ Processing GBIF occurrences for Trametopsis brasiliensis
#> ■■■■■■■■                          22% | ETA:  8s
#> ℹ Processing GBIF occurrences for Basidiodendron eyrei
#> ■■■■■■■■                          22% | ETA:  8s

#> ℹ Processing GBIF occurrences for Sistotrema oblongisporum
#> ■■■■■■■■                          22% | ETA:  8s

#> ℹ Processing GBIF occurrences for Fomes fomentarius
#> ■■■■■■■■                          22% | ETA:  8s

#> ℹ Processing GBIF occurrences for Mycena renati
#> ■■■■■■■■                          22% | ETA:  8s

#> ℹ Processing GBIF occurrences for Helicogloea pellucida
#> ■■■■■■■■                          22% | ETA:  8s

#> ℹ Processing GBIF occurrences for Radulomyces molaris
#> ■■■■■■■■                          22% | ETA:  8s

#> ℹ Processing GBIF occurrences for Elmerina caryae
#> ■■■■■■■■                          22% | ETA:  8s

#> ■■■■■■■■■■■■■■■■■                 52% | ETA:  5s
#> ℹ Processing GBIF occurrences for Phanerochaete livescens
#> ■■■■■■■■■■■■■■■■■                 52% | ETA:  5s

#> ℹ Processing GBIF occurrences for Gloeohypochnicium analogum
#> ■■■■■■■■■■■■■■■■■                 52% | ETA:  5s

#> ℹ Processing GBIF occurrences for Hyphoderma roseocremeum
#> ■■■■■■■■■■■■■■■■■                 52% | ETA:  5s

#> ℹ Processing GBIF occurrences for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■                 52% | ETA:  5s

#> ℹ Processing GBIF occurrences for Trametes versicolor
#> ■■■■■■■■■■■■■■■■■                 52% | ETA:  5s

#> ℹ Processing GBIF occurrences for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■                 52% | ETA:  5s

#> ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  2s
#> ℹ Processing GBIF occurrences for Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  2s

#> ℹ Processing GBIF occurrences for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  2s

#> ℹ Processing GBIF occurrences for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  2s

#> ℹ Processing GBIF occurrences for Laetisaria buckii
#> ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  2s

#> ℹ Processing GBIF occurrences for Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  2s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Processing GBIF occurrences for Xylodon flaviporus
#> # A tibble: 23 × 55
#> # Groups:   canonicalName [23]
#>    canonicalName  `2024` `2025` `2023` `2022` `2021` `2012` `2013` `2005` `2001`
#>    <chr>           <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
#>  1 Stereum ostrea    889    690    689    279    183    176    147    129    118
#>  2 Xylodon radul…     89     NA     NA     NA     NA     81     NA     90     NA
#>  3 Stereum hirsu…   5638   3211   6508   4822   4686   2475     NA     NA     NA
#>  4 Trametopsis b…     NA     NA     NA     NA     NA     NA     NA     NA     NA
#>  5 Basidiodendro…     NA     NA     NA     NA     NA     NA     29     NA     NA
#>  6 Sistotrema ob…     NA     NA     NA     NA    170     92    328     NA     NA
#>  7 Fomes fomenta…  18624  13612  18436  17309  13005     NA     NA     NA     NA
#>  8 Mycena renati     424    278    375    303    273     NA     NA     NA     NA
#>  9 Helicogloea p…     NA     NA      2     NA     NA     NA     NA     NA     NA
#> 10 Radulomyces m…     81     NA     96     83     NA     NA     NA     NA     NA
#> # ℹ 13 more rows
#> # ℹ 45 more variables: `2004` <int>, `2019` <int>, `2007` <int>, `2006` <int>,
#> #   `2020` <int>, `2018` <int>, `1989` <int>, `2016` <int>, `2017` <int>,
#> #   `1992` <int>, `1979` <int>, `1990` <int>, `1999` <int>, `1997` <int>,
#> #   `1939` <int>, `1936` <int>, `1998` <int>, `1956` <int>, `2000` <int>,
#> #   `2014` <int>, `2015` <int>, `2009` <int>, `1941` <int>, `2010` <int>,
#> #   `2008` <int>, `1901` <int>, `2011` <int>, `1975` <int>, `2003` <int>, …

# Using taxnames vector (returns a tibble)
tax_gbif_occur_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
#> ■■■■■■■■■■■■■■■■                  50% | ETA:  0s
#> ℹ Processing GBIF occurrences for Amanita muscaria
#> ■■■■■■■■■■■■■■■■                  50% | ETA:  0s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Processing GBIF occurrences for Boletus edulis
#> # A tibble: 2 × 2
#>   Global_occurences canonicalName   
#>               <int> <chr>           
#> 1            270184 Amanita muscaria
#> 2             68866 Boletus edulis  
ggplot(
  data_fungi_mini_cleanNames@tax_table,
  aes(y = log10(as.numeric(Global_occurences)), x = currentCanonicalSimple)
) +
  geom_col() +
  geom_col(aes(y = -log10(as.numeric(FR))), fill = "blue") +
  coord_flip() +
  xlab("Number of occurences (log10 scale) at global (grey) scale and in France (blue)")
#> Error in geom_col(): Problem while computing aesthetics.
#> ℹ Error occurred in the 1st layer.
#> Caused by error:
#> ! object 'Global_occurences' not found
# }
```
