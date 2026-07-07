# taxinfo

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

The **taxinfo** package provides comprehensive tools for augmenting
phyloseq objects with taxonomic-based information from various external
data sources. It seamlessly integrates data from GBIF, Wikipedia, GLOBI,
OpenAlex, TAXREF, and other databases to enrich your taxonomic analyses.

## Overview

**taxinfo** is designed to work with phyloseq objects and provides
functions to:

- **Verify and clean taxonomic names** using the Global Names
  Architecture (GNA)
- **Retrieve occurrence data** from GBIF and other biodiversity
  databases
- **Access taxonomic traits** from various databases including
  FungalTraits
- **Get Wikipedia information** including page views, links, and content
  statistics
- **Fetch scientific literature data** from OpenAlex
- **Access interaction data** from GLOBI (Global Biotic Interactions)
- **Validate geographic occurrences** against ecoregions and
  biogeographic regions
- **Retrieve taxonomic photos** and media information

## Installation

You can install the stable version of taxinfo from CRAN:

``` r
install.packages("taxinfo")
```

Or the development version from GitHub:

``` r
# Install from GitHub
devtools::install_github("adrientaudiere/taxinfo")

# Or using pak
pak::pkg_install("adrientaudiere/taxinfo")
```

## Key Features

### 🔍 **Data Verification & Quality Control**

- [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md):
  Verify and standardize taxonomic names using Global Names Architecture

### 🌍 **Biodiversity Data Integration**

- [`tax_gbif_occur_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_occur_pq.md):
  Retrieve GBIF occurrence data
- [`tax_globi_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_globi_pq.md):
  Access species interaction data from GLOBI
- [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md):
  Add information from CSV files (TAXREF, traits databases)

### 📚 **Knowledge Base Integration**

- [`tax_get_wk_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_info_pq.md):
  Get comprehensive Wikipedia data
- [`tax_oa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_oa_pq.md):
  Retrieve scientific literature from OpenAlex

### 🗺️ **Geographic Analysis**

- [`range_bioreg_pq()`](https://adrientaudiere.github.io/taxinfo/reference/range_bioreg_pq.md):
  Analyze biogeographic ranges
- [`plot_tax_gbif_pq()`](https://adrientaudiere.github.io/taxinfo/reference/plot_tax_gbif_pq.md):
  Create distribution maps
- [`tax_check_ecoregion()`](https://adrientaudiere.github.io/taxinfo/reference/tax_check_ecoregion.md):
  Validate occurrences against ecoregions

### 🔬 **Advanced Analysis Tools**

- [`tax_retroblast_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_retroblast_pq.md):
  Sequence-based taxonomic verification
- [`tax_photos_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_photos_pq.md):
  Access taxonomic images and media
- [`tax_occur_check_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_check_pq.md):
  Multi-source occurrence validation

### 🎯 **Flexible Input Options**

Most functions can work with either: - **Phyloseq objects**:
Automatically enriches the tax_table (default behavior) - **Taxonomic
name vectors**: Returns tibbles for standalone queries

## Quick Start

### Glossary

Following the [Darwin core standards](https://dwc.tdwg.org/terms/), here
are some key terms used in taxinfo (camel case naming convention): -
**scientificName**: The full scientific name with authorship and date
information if known (e.g., “Stereum ostrea (Blume & T.Nees) Fr.,
1838”) - **genusEpithet**: Just the genus part (e.g., “Stereum”). Note
that the correct Darwin core term is `genus`, but in taxinfo we use
`genusEpithet` to avoid confusion with the `genus` field from phyloseq
which is often used for the full scientific name. - **specificEpithet**:
Just the species epithet part (e.g., “ostrea”) -
**namePublishedInYear**: The year the name was published (e.g., “1838”)

Other term come from [verifier
globalnames](https://verifier.globalnames.org/) (camel case naming
convention): - **currentCanonicalSimple**: The simplified scientific
name without authorship (e.g., “Stereum ostrea”). It correspond to
concatenation of the `genusEpithet` and `specificEpithet` fields.

- We add the terms **genusSpeciesEpithet**: Same as
  `currentCanonicalSimple` but `NA` for genus-only names (i.e. when
  `specificEpithet` is absent). Useful when you need to filter out
  unidentified-to-species taxa.

### Example Workflow

``` r
library(taxinfo)
#> Le chargement a nécessité le package : MiscMetabar
#> Le chargement a nécessité le package : phyloseq
#> Le chargement a nécessité le package : ggplot2
#> Le chargement a nécessité le package : dplyr
#> 
#> Attachement du package : 'dplyr'
#> Les objets suivants sont masqués depuis 'package:stats':
#> 
#>     filter, lag
#> Les objets suivants sont masqués depuis 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(MiscMetabar)

# Load example data (fungal phyloseq object from MiscMetabar)
data("data_fungi_mini", package = "MiscMetabar")

# Step 1: Verify and clean taxonomic names
data_fungi_clean <- gna_verifier_pq(data_fungi_mini,
  data_sources = 210
)
#> ℹ Some GNA `data_sources` are older than 365 days; name resolution may miss
#>   recent taxa:
#>   TAXREF (id 210): last updated 2025-04-02
#> ℹ Compare update dates at <https://verifier.globalnames.org/data_sources>.
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 45
#> • Taxa submitted for verification: 37
#> • Genus-level only taxa: 2
#> • Total matches found: 25
#> • Synonyms: 4 (including 0 uninomial)
#> • Accepted names: 21 (including 6 uninomial)
#> ℹ 6 uninomial accepted name(s) have `currentCanonicalSimple` set to "NA"
#>   (`species_only` = TRUE)

# Step 2: Add GBIF occurrence data (add_to_phyloseq defaults to TRUE)
data_with_gbif <- tax_gbif_occur_pq(data_fungi_clean)
#> ℹ Processing GBIF occurrences for Stereum ostrea
#> ℹ Processing GBIF occurrences for Ossicaulis lachnopus
#> ■■■■■■                            17% | ETA:  6s
#> ℹ Processing GBIF occurrences for Stereum hirsutum
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Basidiodendron eyrei
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Sistotrema oblongisporum
#> ■■■■■■                            17% | ETA:  6s■■■■■■■■■■■                       33% | ETA:  6s
#> ℹ Processing GBIF occurrences for Fomes fomentarius
#> ■■■■■■■■■■■                       33% | ETA:  6sℹ Processing GBIF occurrences for Cerocorticium molare
#> ■■■■■■■■■■■                       33% | ETA:  6sℹ Processing GBIF occurrences for Aporpium canescens
#> ■■■■■■■■■■■                       33% | ETA:  6sℹ Processing GBIF occurrences for Hypochnicium analogum
#> ■■■■■■■■■■■                       33% | ETA:  6sℹ Processing GBIF occurrences for Hyphoderma roseocremeum
#> ■■■■■■■■■■■                       33% | ETA:  6s■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> ℹ Processing GBIF occurrences for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4sℹ Processing GBIF occurrences for Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4sℹ Processing GBIF occurrences for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4sℹ Processing GBIF occurrences for Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4sℹ Processing GBIF occurrences for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4sℹ Processing GBIF occurrences for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ℹ Processing GBIF occurrences for Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ Processing GBIF occurrences for Xylodon flaviporus

# Step 3: Add trait information
fungal_traits <- system.file("extdata", "fun_trait_mini.csv", package = "taxinfo")
data_with_traits <- tax_info_pq(data_with_gbif,
  taxonomic_rank = "genusEpithet",
  file_name = fungal_traits,
  csv_taxonomic_rank = "GENUS",
  col_prefix = "ft_",
  sep = ";"
)
#> ✔ Added 18 columns from '/tmp/RtmptJgQnw/temp_libpath22f79458dc01b9/taxinfo/extdata/fun_trait_mini.csv' with information for 0 taxa in the tax_table slot of the phyloseq object

# Step 4: Add Wikipedia information (add_to_phyloseq defaults to TRUE)
data_final <- tax_get_wk_info_pq(data_with_traits)
#> ℹ Getting taxonomic IDs from Wikidata...
#> ℹ Getting page views from Wikipedia for Stereum ostrea
#> ■■■■                              11% | ETA:  1m
#> ℹ Getting page views from Wikipedia for Ossicaulis lachnopus
#> ■■■■                              11% | ETA:  1m■■■■■■                            16% | ETA:  1m
#> ℹ Getting page views from Wikipedia for Stereum hirsutum
#> ■■■■■■                            16% | ETA:  1m■■■■■■■                           21% | ETA:  2m
#> ℹ Getting page views from Wikipedia for Basidiodendron eyrei
#> ■■■■■■■                           21% | ETA:  2m■■■■■■■■■                         26% | ETA:  1m
#> ℹ Getting page views from Wikipedia for Sistotrema oblongisporum
#> ■■■■■■■■■                         26% | ETA:  1m■■■■■■■■■■                        32% | ETA:  1m
#> ℹ Getting page views from Wikipedia for Fomes fomentarius
#> ■■■■■■■■■■                        32% | ETA:  1m■■■■■■■■■■■■                      37% | ETA:  2m
#> ℹ Getting page views from Wikipedia for Mycena renatii
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Cerocorticium molare
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Aporpium canescens
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Hypochnicium analogum
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Hyphoderma roseocremeum
#> ■■■■■■■■■■■■                      37% | ETA:  2m■■■■■■■■■■■■■■■■■■■■              63% | ETA: 39s
#> ℹ Getting page views from Wikipedia for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 39s■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 32s
#> ℹ Getting page views from Wikipedia for Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 32s■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 35s
#> ℹ Getting page views from Wikipedia for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 35s■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 27s
#> ℹ Getting page views from Wikipedia for Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 27s■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 22s
#> ℹ Getting page views from Wikipedia for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 22s■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA: 14s
#> ℹ Getting page views from Wikipedia for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA: 14s■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  7s
#> ℹ Getting page views from Wikipedia for Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  7s■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ Getting page views from Wikipedia for Xylodon flaviporus

# View the enriched taxonomic table
head(data_final@tax_table)
#> Taxonomy Table:     [6 taxa by 47 taxonomic ranks]:
#>       ft_GENUS     ft_Source ft_COMMENT.on.genus ft_primary_lifestyle
#> ASV7  "NA"         NA        NA                  NA                  
#> ASV8  "Stereum"    NA        NA                  NA                  
#> ASV12 "Xylodon"    NA        NA                  NA                  
#> ASV18 "Stereum"    NA        NA                  NA                  
#> ASV25 "Ossicaulis" NA        NA                  NA                  
#> ASV26 "Stereum"    NA        NA                  NA                  
#>       ft_Secondary_lifestyle ft_Comment_on_lifestyle_template
#> ASV7  NA                     NA                              
#> ASV8  NA                     NA                              
#> ASV12 NA                     NA                              
#> ASV18 NA                     NA                              
#> ASV25 NA                     NA                              
#> ASV26 NA                     NA                              
#>       ft_Endophytic_interaction_capability_template
#> ASV7  NA                                           
#> ASV8  NA                                           
#> ASV12 NA                                           
#> ASV18 NA                                           
#> ASV25 NA                                           
#> ASV26 NA                                           
#>       ft_Plant_pathogenic_capacity_template ft_Decay_substrate_template
#> ASV7  NA                                    NA                         
#> ASV8  NA                                    NA                         
#> ASV12 NA                                    NA                         
#> ASV18 NA                                    NA                         
#> ASV25 NA                                    NA                         
#> ASV26 NA                                    NA                         
#>       ft_Decay_type_template ft_Aquatic_habitat_template
#> ASV7  NA                     NA                         
#> ASV8  NA                     NA                         
#> ASV12 NA                     NA                         
#> ASV18 NA                     NA                         
#> ASV25 NA                     NA                         
#> ASV26 NA                     NA                         
#>       ft_Animal_biotrophic_capacity_template ft_Specific_hosts
#> ASV7  NA                                     NA               
#> ASV8  NA                                     NA               
#> ASV12 NA                                     NA               
#> ASV18 NA                                     NA               
#> ASV25 NA                                     NA               
#> ASV26 NA                                     NA               
#>       ft_Growth_form_template ft_Fruitbody_type_template
#> ASV7  NA                      NA                        
#> ASV8  NA                      NA                        
#> ASV12 NA                      NA                        
#> ASV18 NA                      NA                        
#> ASV25 NA                      NA                        
#> ASV26 NA                      NA                        
#>       ft_Hymenium_type_template ft_Ectomycorrhiza_exploration_type_template
#> ASV7  NA                        NA                                         
#> ASV8  NA                        NA                                         
#> ASV12 NA                        NA                                         
#> ASV18 NA                        NA                                         
#> ASV25 NA                        NA                                         
#> ASV26 NA                        NA                                         
#>       ft_Ectomycorrhiza_lineage_template ft_primary_photobiont
#> ASV7  NA                                 NA                   
#> ASV8  NA                                 NA                   
#> ASV12 NA                                 NA                   
#> ASV18 NA                                 NA                   
#> ASV25 NA                                 NA                   
#> ASV26 NA                                 NA                   
#>       ft_secondary_photobiont Domain  Phylum          Class           
#> ASV7  NA                      "Fungi" "Basidiomycota" "Agaricomycetes"
#> ASV8  NA                      "Fungi" "Basidiomycota" "Agaricomycetes"
#> ASV12 NA                      "Fungi" "Basidiomycota" "Agaricomycetes"
#> ASV18 NA                      "Fungi" "Basidiomycota" "Agaricomycetes"
#> ASV25 NA                      "Fungi" "Basidiomycota" "Agaricomycetes"
#> ASV26 NA                      "Fungi" "Basidiomycota" "Agaricomycetes"
#>       Order             Family           Genus        Species      Trophic.Mode
#> ASV7  "Russulales"      "Stereaceae"     NA           NA           "Saprotroph"
#> ASV8  "Russulales"      "Stereaceae"     "Stereum"    "ostrea"     "Saprotroph"
#> ASV12 "Hymenochaetales" "Schizoporaceae" "Xylodon"    "raduloides" "Saprotroph"
#> ASV18 "Russulales"      "Stereaceae"     "Stereum"    "ostrea"     "Saprotroph"
#> ASV25 "Agaricales"      "Lyophyllaceae"  "Ossicaulis" "lachnopus"  "Saprotroph"
#> ASV26 "Russulales"      "Stereaceae"     "Stereum"    "hirsutum"   "Saprotroph"
#>       Guild                                  Trait       Confidence.Ranking
#> ASV7  "Wood Saprotroph-Undefined Saprotroph" "NULL"      "Probable"        
#> ASV8  "Undefined Saprotroph"                 "White Rot" "Probable"        
#> ASV12 "Undefined Saprotroph"                 "White Rot" "Probable"        
#> ASV18 "Undefined Saprotroph"                 "White Rot" "Probable"        
#> ASV25 "Wood Saprotroph"                      "Brown Rot" "Probable"        
#> ASV26 "Undefined Saprotroph"                 "White Rot" "Probable"        
#>       Genus_species          currentName                                
#> ASV7  "NA_NA"                NA                                         
#> ASV8  "Stereum_ostrea"       "Stereum ostrea (Blume & T.Nees) Fr., 1838"
#> ASV12 "Xylodon_raduloides"   "Xylodon (Pers.) Gray, 1821"               
#> ASV18 "Stereum_ostrea"       "Stereum ostrea (Blume & T.Nees) Fr., 1838"
#> ASV25 "Ossicaulis_lachnopus" "Ossicaulis lachnopus (Fr.) Contu, 2000"   
#> ASV26 "Stereum_hirsutum"     "Stereum hirsutum (Willd.) Pers., 1800"    
#>       currentCanonicalSimple genusEpithet specificEpithet
#> ASV7  NA                     NA           NA             
#> ASV8  "Stereum ostrea"       "Stereum"    "ostrea"       
#> ASV12 NA                     "Xylodon"    NA             
#> ASV18 "Stereum ostrea"       "Stereum"    "ostrea"       
#> ASV25 "Ossicaulis lachnopus" "Ossicaulis" "lachnopus"    
#> ASV26 "Stereum hirsutum"     "Stereum"    "hirsutum"     
#>       genusSpeciesEpithet    namePublishedInYear authorship bracketauthorship
#> ASV7  NA                     NA                  NA         NA               
#> ASV8  "Stereum ostrea"       "1838"              "Fr."      "Blume & T.Nees" 
#> ASV12 NA                     "1821"              "Gray"     "Pers."          
#> ASV18 "Stereum ostrea"       "1838"              "Fr."      "Blume & T.Nees" 
#> ASV25 "Ossicaulis lachnopus" "2000"              "Contu"    "Fr."            
#> ASV26 "Stereum hirsutum"     "1800"              "Pers."    "Willd."         
#>       scientificNameAuthorship Global_occurences lang page_length page_views
#> ASV7  NA                       NA                NA   NA          NA        
#> ASV8  "(Blume & T.Nees) Fr."   " 11562"          " 9" "5519.600"  " 904"    
#> ASV12 "(Pers.) Gray"           NA                NA   NA          NA        
#> ASV18 "(Blume & T.Nees) Fr."   " 11562"          " 9" "5519.600"  " 904"    
#> ASV25 "(Fr.) Contu"            "   227"          " 4" "2340.000"  "   0"    
#> ASV26 "(Willd.) Pers."         "123141"          "25" "4703.333"  "   0"    
#>       taxon_id    taxa_name             
#> ASV7  NA          "NA"                  
#> ASV8  "Q2710042"  "Stereum ostrea"      
#> ASV12 NA          "NA"                  
#> ASV18 "Q2710042"  "Stereum ostrea"      
#> ASV25 "Q10613125" "Ossicaulis lachnopus"
#> ASV26 "Q557377"   "Stereum hirsutum"

# Alternative: Query specific taxa without a phyloseq object
taxa_info <- tax_gbif_occur_pq(
  taxnames = c("Amanita muscaria", "Boletus edulis"),
  by_country = TRUE
)
#> ℹ Processing GBIF occurrences for Amanita muscaria
#> ℹ Processing GBIF occurrences for Boletus edulis

# Returns a tibble instead of phyloseq object
head(taxa_info)
#> # A tibble: 2 × 13
#> # Groups:   query_name [2]
#>   query_name      NL    US    GB    DE    CA    SE    DK    RU    AU    AT    NO
#>   <chr>        <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
#> 1 Amanita mu… 129952 31671 26232 24548 10173  9166  9008  8815  7876  7756    NA
#> 2 Boletus ed…   5824  5867 10727  4835    NA 12980  5966  4156    NA  3868  4584
#> # ℹ 1 more variable: CH <int>
```

## Data Sources

**taxinfo** integrates with multiple authoritative data sources:

| Source         | Description                                     | Functions                                                                                                                                                                                                    |
|----------------|-------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **GBIF**       | Global biodiversity occurrence data             | [`tax_gbif_occur_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_occur_pq.md), [`plot_tax_gbif_pq()`](https://adrientaudiere.github.io/taxinfo/reference/plot_tax_gbif_pq.md)             |
| **Wikipedia**  | Encyclopedia data and page statistics           | [`tax_get_wk_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_info_pq.md), [`tax_get_wk_pages_info()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_pages_info.md) |
| **GLOBI**      | Species interaction networks                    | [`tax_globi_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_globi_pq.md)                                                                                                                       |
| **OpenAlex**   | Scientific literature database                  | [`tax_oa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_oa_pq.md)                                                                                                                             |
| **TAXREF**     | French national taxonomic reference             | [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md)                                                                                                                         |
| **GNA**        | Global Names Architecture for name verification | [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md)                                                                                                                 |
| **Custom CSV** | Any taxonomic database in CSV format            | [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md)                                                                                                                         |

## Contributing

We welcome contributions! Please open an issue or pull request on
[GitHub](https://github.com/adrientaudiere/taxinfo).

## Citation

If you use taxinfo in your research, please cite:

``` r
citation("taxinfo")
```

## Related Packages

**taxinfo** works seamlessly with:

- [MiscMetabar](https://github.com/adrientaudiere/MiscMetabar):
  Miscellaneous functions for metabarcoding analysis
- [phyloseq](https://joey711.github.io/phyloseq/): Analyze microbiome
  census data
- [taxize](https://github.com/ropensci/taxize): Taxonomic information
  from around the web
- [rgbif](https://github.com/ropensci/rgbif): Interface to GBIF API

## Licence

This project is licensed under the MIT License - see the
[LICENSE](https://adrientaudiere.github.io/taxinfo/LICENSE) file for
details.

## Acknowledgments

- All the members of the DEFIS MITI project, especially Mélanie Roy and
  Benoît Perez-Lamarque who lead the project.

- This project has received financial support from the **CNRS** (*Centre
  National de la Recherche Scientifique*) through the MITI
  interdisciplinary programs (Project ***DEFIS*** - Exploration of
  Evolutionary Diversity of Fungi and its Indicators through
  High-Throughput Sequencing : from multi-actors challenges to long-term
  monitoring).

- The developers of the R packages used in this project. A special
  thanks to Joey McMurdie (`phyloseq`), John Waller (`rgbif`), and
  Zachary Foster (`taxize`) for maintaining those useful tools.
