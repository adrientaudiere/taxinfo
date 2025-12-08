# Extract taxonomic names from a phyloseq object

Mainly a internal function for function \[gna_verifier_pq()\],
\[tax_oa_pq()\], \[gbif_occur_pq()\], \[tax_iucn_code_pq()\],
\[tax_globi_pq()\], \[plot_tax_gbif_pq()\], ...

## Usage

``` r
taxonomic_rank_to_taxnames(
  physeq,
  taxonomic_rank = c("Genus", "Species"),
  discard_genus_alone = FALSE,
  discard_NA = TRUE,
  distinct_names = TRUE
)
```

## Arguments

- physeq:

  A phyloseq object

- taxonomic_rank:

  (Character) The column(s) present in the @tax_table slot of the
  phyloseq object. Can be a vector of two columns (e.g. the default
  c("Genus", "Species")).

- discard_genus_alone:

  (logical default FALSE). If TRUE genus without information at the
  species level are discarded.

- discard_NA:

  (logical default TRUE). If TRUE, taxa with NA in the taxonomic_rank
  are discarded.

- distinct_names:

  (logical default TRUE). If TRUE, return only unique taxonomic names.

## Value

A vector of unique taxonomic names

## Author

Adrien Taudiere

## Examples

``` r
taxonomic_rank_to_taxnames(data_fungi_mini)
#>  [1] "Stereum ostrea"             "Xylodon raduloides"        
#>  [3] "Ossicaulis lachnopus"       "Stereum hirsutum"          
#>  [5] "Antrodiella brasiliensis"   "Basidiodendron eyrei"      
#>  [7] "Sistotrema oblongisporum"   "Entocybe"                  
#>  [9] "Fomes fomentarius"          "Mycena renati"             
#> [11] "Helicogloea pellucida"      "Radulomyces molaris"       
#> [13] "Elmerina caryae"            "Phanerochaete livescens"   
#> [15] "Gloeohypochnicium analogum" "Auricularia"               
#> [17] "Hyphoderma roseocremeum"    "Hyphoderma setigerum"      
#> [19] "Trametes versicolor"        "Peniophora versiformis"    
#> [21] "Exidia glandulosa"          "Peniophorella pubera"      
#> [23] "Auricularia mesenterica"    "Marchandiomyces buckii"    
#> [25] "Hericium coralloides"       "Xylodon flaviporus"        
taxonomic_rank_to_taxnames(data_fungi_mini, discard_genus_alone = TRUE)
#>  [1] "Stereum ostrea"             "Xylodon raduloides"        
#>  [3] "Ossicaulis lachnopus"       "Stereum hirsutum"          
#>  [5] "Antrodiella brasiliensis"   "Basidiodendron eyrei"      
#>  [7] "Sistotrema oblongisporum"   "Fomes fomentarius"         
#>  [9] "Mycena renati"              "Helicogloea pellucida"     
#> [11] "Radulomyces molaris"        "Elmerina caryae"           
#> [13] "Phanerochaete livescens"    "Gloeohypochnicium analogum"
#> [15] "Hyphoderma roseocremeum"    "Hyphoderma setigerum"      
#> [17] "Trametes versicolor"        "Peniophora versiformis"    
#> [19] "Exidia glandulosa"          "Peniophorella pubera"      
#> [21] "Auricularia mesenterica"    "Marchandiomyces buckii"    
#> [23] "Hericium coralloides"       "Xylodon flaviporus"        
taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE)
#>  [1] "Stereum ostrea"             "Xylodon raduloides"        
#>  [3] "Ossicaulis lachnopus"       "Stereum hirsutum"          
#>  [5] "Antrodiella brasiliensis"   "Basidiodendron eyrei"      
#>  [7] "Sistotrema oblongisporum"   "Entocybe"                  
#>  [9] "Fomes fomentarius"          "Mycena renati"             
#> [11] "Helicogloea pellucida"      "Radulomyces molaris"       
#> [13] "Elmerina caryae"            "Phanerochaete livescens"   
#> [15] "Gloeohypochnicium analogum" "Auricularia"               
#> [17] "Hyphoderma roseocremeum"    "Hyphoderma setigerum"      
#> [19] "Trametes versicolor"        "Peniophora versiformis"    
#> [21] "Exidia glandulosa"          "Peniophorella pubera"      
#> [23] "Auricularia mesenterica"    "Marchandiomyces buckii"    
#> [25] "Hericium coralloides"       "Xylodon flaviporus"        
taxonomic_rank_to_taxnames(data_fungi_mini,
  discard_NA = TRUE, discard_genus_alone = TRUE
)
#>  [1] "Stereum ostrea"             "Xylodon raduloides"        
#>  [3] "Ossicaulis lachnopus"       "Stereum hirsutum"          
#>  [5] "Antrodiella brasiliensis"   "Basidiodendron eyrei"      
#>  [7] "Sistotrema oblongisporum"   "Fomes fomentarius"         
#>  [9] "Mycena renati"              "Helicogloea pellucida"     
#> [11] "Radulomyces molaris"        "Elmerina caryae"           
#> [13] "Phanerochaete livescens"    "Gloeohypochnicium analogum"
#> [15] "Hyphoderma roseocremeum"    "Hyphoderma setigerum"      
#> [17] "Trametes versicolor"        "Peniophora versiformis"    
#> [19] "Exidia glandulosa"          "Peniophorella pubera"      
#> [21] "Auricularia mesenterica"    "Marchandiomyces buckii"    
#> [23] "Hericium coralloides"       "Xylodon flaviporus"        
```
