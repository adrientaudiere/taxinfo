# Getting Started with taxinfo: Augment phyloseq Objects with Taxonomic Information

🛈

×

**Messages**

``` popup-pre
#> Loading required package: MiscMetabar
#> Loading required package: phyloseq
#> Loading required package: ggplot2
#> Loading required package: dada2
#> Loading required package: Rcpp
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: purrr
```

``` r
library(taxinfo)
```

## Overview

The **taxinfo** package provides comprehensive tools for augmenting
phyloseq objects with taxonomic-based information from various external
data sources. This vignette provides a big picture overview of the
package’s capabilities and workflow. A large part of ASV or OTUs,
hereafter referred to as `taxa`, obtained from metabarcoding studies are
often not identified at the species level. But those taxa with a genus
or species level identification can still be enriched with valuable
information from various databases. The **taxinfo** package allows you
to easily integrate such information into your phyloseq objects.
**Taxinfo** also create a body of evidence for the presence of a species
in your samples by testing the likelihood of each taxon being present.

## Key Features

**taxinfo** integrates with multiple authoritative data sources to
enrich your taxonomic data:

- **🔍 Scientific Names Verification**: Verify and standardize taxonomic
  names using Global Names Architecture
- **🌍 Biodiversity Data**: Access GBIF occurrence data and species
  interactions from GLOBI  
- **📚 Knowledge Integration**: Retrieve Wikipedia data and scientific
  literature from OpenAlex
- **🗺️ Geographic Analysis**: Analyze biogeographic ranges and create
  distribution maps

## Main Data Sources

| Source | Description | Key Functions |
|----|----|----|
| **GBIF** | Global biodiversity occurrence data | [`tax_gbif_occur_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_occur_pq.md), [`plot_tax_gbif_pq()`](https://adrientaudiere.github.io/taxinfo/reference/plot_tax_gbif_pq.md) |
| **Wikipedia** | Encyclopedia data and page statistics | [`tax_get_wk_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_info_pq.md) |
| **GLOBI** | Species interaction networks | [`tax_globi_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_globi_pq.md) |
| **OpenAlex** | Scientific literature database | [`tax_oa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_oa_pq.md) |
| **GNA** | Global Names Architecture for name verification | [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md) |
| **Custom CSV** | Any database in CSV format with a column documenting Taxonomic information | [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md) |

## Basic Workflow

### Step 1: Load Example Data

``` r
# Load example fungal data from MiscMetabar
data("data_fungi_mini", package = "MiscMetabar")

# Check the structure
data_fungi_mini
```

    #> phyloseq-class experiment-level object
    #> otu_table()   OTU Table:         [ 45 taxa and 137 samples ]
    #> sample_data() Sample Data:       [ 137 samples by 7 sample variables ]
    #> tax_table()   Taxonomy Table:    [ 45 taxa by 12 taxonomic ranks ]
    #> refseq()      DNAStringSet:      [ 45 reference sequences ]

### Step 2: Verify and Clean Taxonomic Names

The first step is typically to verify and standardize taxonomic names
using the Global Names Architecture:

🛈

×

**Messages**

``` popup-pre
#>  [32m✔ [39m GNA verification summary:
#>  [36m• [39m Total taxa in phyloseq:  [34m20 [39m
#>  [36m• [39m Taxa submitted for verification:  [34m19 [39m
#>  [36m• [39m Genus-level only taxa:  [34m2 [39m
#>  [36m• [39m Total matches found:  [34m15 [39m
#>  [36m• [39m Synonyms:  [34m4 [39m (including  [34m4 [39m at genus level)
#>  [36m• [39m Accepted names:  [34m11 [39m (including  [34m6 [39m at genus level)
```

``` r
# Keep only first 20 taxa for speed
data_clean <- prune_taxa(taxa = taxa_names(data_fungi_mini)[1:20], data_fungi_mini) |>
  gna_verifier_pq(data_sources = 210)
```

``` r

# View the enhanced taxonomic table
head(data_clean@tax_table)
```

    #> Taxonomy Table:     [6 taxa by 21 taxonomic ranks]:
    #>       Domain  Phylum          Class            Order            
    #> ASV7  "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"     
    #> ASV8  "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"     
    #> ASV12 "Fungi" "Basidiomycota" "Agaricomycetes" "Hymenochaetales"
    #> ASV18 "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"     
    #> ASV25 "Fungi" "Basidiomycota" "Agaricomycetes" "Agaricales"     
    #> ASV26 "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"     
    #>       Family           Genus        Species      Trophic.Mode
    #> ASV7  "Stereaceae"     NA           NA           "Saprotroph"
    #> ASV8  "Stereaceae"     "Stereum"    "ostrea"     "Saprotroph"
    #> ASV12 "Schizoporaceae" "Xylodon"    "raduloides" "Saprotroph"
    #> ASV18 "Stereaceae"     "Stereum"    "ostrea"     "Saprotroph"
    #> ASV25 "Lyophyllaceae"  "Ossicaulis" "lachnopus"  "Saprotroph"
    #> ASV26 "Stereaceae"     "Stereum"    "hirsutum"   "Saprotroph"
    #>       Guild                                  Trait       Confidence.Ranking
    #> ASV7  "Wood Saprotroph-Undefined Saprotroph" "NULL"      "Probable"        
    #> ASV8  "Undefined Saprotroph"                 "White Rot" "Probable"        
    #> ASV12 "Undefined Saprotroph"                 "White Rot" "Probable"        
    #> ASV18 "Undefined Saprotroph"                 "White Rot" "Probable"        
    #> ASV25 "Wood Saprotroph"                      "Brown Rot" "Probable"        
    #> ASV26 "Undefined Saprotroph"                 "White Rot" "Probable"        
    #>       Genus_species          taxa_name             
    #> ASV7  "NA_NA"                ""                    
    #> ASV8  "Stereum_ostrea"       "Stereum ostrea"      
    #> ASV12 "Xylodon_raduloides"   "Xylodon raduloides"  
    #> ASV18 "Stereum_ostrea"       "Stereum ostrea"      
    #> ASV25 "Ossicaulis_lachnopus" "Ossicaulis lachnopus"
    #> ASV26 "Stereum_hirsutum"     "Stereum hirsutum"    
    #>       currentName                                 currentCanonicalSimple
    #> ASV7  NA                                          NA                    
    #> ASV8  "Stereum ostrea (Blume & T.Nees) Fr., 1838" "Stereum ostrea"      
    #> ASV12 "Xylodon (Pers.) Gray, 1821"                "Xylodon"             
    #> ASV18 "Stereum ostrea (Blume & T.Nees) Fr., 1838" "Stereum ostrea"      
    #> ASV25 "Ossicaulis lachnopus (Fr.) Contu, 2000"    "Ossicaulis lachnopus"
    #> ASV26 "Stereum hirsutum (Willd.) Pers., 1800"     "Stereum hirsutum"    
    #>       genusEpithet specificEpithet namePublishedInYear authorship
    #> ASV7  NA           NA              NA                  NA        
    #> ASV8  "Stereum"    "ostrea"        "1838"              "Fr."     
    #> ASV12 "Xylodon"    NA              "1821"              "Gray"    
    #> ASV18 "Stereum"    "ostrea"        "1838"              "Fr."     
    #> ASV25 "Ossicaulis" "lachnopus"     "2000"              "Contu"   
    #> ASV26 "Stereum"    "hirsutum"      "1800"              "Pers."   
    #>       bracketauthorship scientificNameAuthorship
    #> ASV7  NA                NA                      
    #> ASV8  "Blume & T.Nees"  "(Blume & T.Nees) Fr."  
    #> ASV12 "Pers."           "(Pers.) Gray"          
    #> ASV18 "Blume & T.Nees"  "(Blume & T.Nees) Fr."  
    #> ASV25 "Fr."             "(Fr.) Contu"           
    #> ASV26 "Willd."          "(Willd.) Pers."

This adds standardized columns: - `taxa_name`: Original name submitted -
`currentName`: Current accepted name with authorities  
- `currentCanonicalSimple`: Clean accepted name without authorities -
`specificEpithet` : Accepted name at species level or NA - `genus` :
Accepted name at Genus level or NA

You can also output only the verified names with more information on the
matching algorithm results as a tibble by setting
`add_to_phyloseq = FALSE`.

🛈⚠

×

**Messages**

``` popup-pre
#>  [32m✔ [39m GNA verification summary:
#>  [36m• [39m Taxa submitted for verification:  [34m26 [39m
#>  [36m• [39m Total matches found:  [34m0 [39m
#>  [36m• [39m Synonyms:  [34m0 [39m (including  [34m0 [39m at genus level)
#>  [36m• [39m Accepted names:  [34m0 [39m (including  [34m0 [39m at genus level)
```

×

**Warnings**

``` popup-pre
#> Warning: Unknown or uninitialised column: `taxonomicStatus`.
#> Unknown or uninitialised column: `taxonomicStatus`.
#> Warning: Unknown or uninitialised column: `matchedCardinality`.
#> Warning: Unknown or uninitialised column: `taxonomicStatus`.
#> Warning: Unknown or uninitialised column: `matchedCardinality`.
```

``` r
df <- gna_verifier_pq(data_fungi_mini,
  data_sources = 210,
  add_to_phyloseq = FALSE
)
```

``` r
glimpse(df)
```

    #> Rows: 26
    #> Columns: 30
    #> $ submittedName          <chr> "Stereum ostrea", "Xylodon raduloides", "Ossica…
    #> $ dataSourceId           <chr> "210", "210", "210", "210", "210", "210", "210"…
    #> $ dataSourceTitleShort   <chr> "TAXREF", "TAXREF", "TAXREF", "TAXREF", "TAXREF…
    #> $ curation               <chr> "Curated", "Curated", "Curated", "Curated", "Cu…
    #> $ recordId               <chr> "900725", "901834", "465057", "44622", "189273"…
    #> $ entryDate              <chr> "2025-04-02", "2025-04-02", "2025-04-02", "2025…
    #> $ sortScore              <dbl> 9.411447, 8.634970, 9.411447, 9.411447, 8.63497…
    #> $ matchedNameID          <chr> "0a43b928-10f0-57d2-8b37-7ce10de25b32", "89b563…
    #> $ matchedName            <chr> "Stereum ostrea (Blume & T.Nees) Fr., 1838", "X…
    #> $ matchedCardinality     <dbl> 2, 1, 2, 2, 1, 2, 2, NA, 2, 2, 1, 2, 2, 1, 2, 1…
    #> $ matchedCanonicalSimple <chr> "Stereum ostrea", "Xylodon", "Ossicaulis lachno…
    #> $ matchedCanonicalFull   <chr> "Stereum ostrea", "Xylodon", "Ossicaulis lachno…
    #> $ currentRecordId        <chr> "900725", "901834", "465057", "44622", "189273"…
    #> $ currentNameId          <chr> "0a43b928-10f0-57d2-8b37-7ce10de25b32", "89b563…
    #> $ currentName            <chr> "Stereum ostrea (Blume & T.Nees) Fr., 1838", "X…
    #> $ currentCardinality     <dbl> 2, 1, 2, 2, 1, 2, 2, NA, 2, 2, 1, 2, 2, 1, 2, 1…
    #> $ currentCanonicalSimple <chr> "Stereum ostrea", "Xylodon", "Ossicaulis lachno…
    #> $ currentCanonicalFull   <chr> "Stereum ostrea", "Xylodon", "Ossicaulis lachno…
    #> $ taxonomicStatus        <chr> "Accepted", "Accepted", "Accepted", "Accepted",…
    #> $ isSynonym              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
    #> $ editDistance           <dbl> 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0…
    #> $ stemEditDistance       <dbl> 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0…
    #> $ matchType              <chr> "Exact", "PartialExact", "Exact", "Exact", "Par…
    #> $ cardinalityScore       <dbl> 1, 0, 1, 1, 0, 1, 1, NA, 1, 1, 0, 1, 1, 0, 1, 1…
    #> $ infraSpecificRankScore <dbl> 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0…
    #> $ fuzzyLessScore         <dbl> 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1…
    #> $ curatedDataScore       <dbl> 0.6666667, 0.6666667, 0.6666667, 0.6666667, 0.6…
    #> $ authorMatchScore       <dbl> 0.1428571, 0.1428571, 0.1428571, 0.1428571, 0.1…
    #> $ acceptedNameScore      <dbl> 1, 1, 1, 1, 1, 1, 1, NA, 1, 0, 1, 0, 0, 1, 0, 1…
    #> $ parsingQualityScore    <dbl> 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1…

### Step 3: Add Biodiversity Information

Once names are verified, you can enrich your data with various sources:

🛈⚠

×

**Messages**

``` popup-pre
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mStereum ostrea [3m [23m
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mOssicaulis lachnopus [3m [23m
#>  [32m■■■■■■■■■■■                      [39m  33% | ETA:  2s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mStereum hirsutum [3m [23m
#>  [32m■■■■■■■■■■■                      [39m  33% | ETA:  2s [36mℹ [39m Processing GBIF occurrences for  [3m [3mBasidiodendron eyrei [3m [23m
#>  [32m■■■■■■■■■■■                      [39m  33% | ETA:  2s [36mℹ [39m Processing GBIF occurrences for  [3m [3mSistotrema oblongisporum [3m [23m
#>  [32m■■■■■■■■■■■                      [39m  33% | ETA:  2s [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  1s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mFomes fomentarius [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  1s [36mℹ [39m Processing GBIF occurrences for  [3m [3mCerocorticium molare [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  1s [36mℹ [39m Processing GBIF occurrences for  [3m [3mAporpium canescens [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  1s [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m 100% | ETA:  0s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mHypochnicium analogum [3m [23m
#>  [36mℹ [39m Getting taxonomic IDs from Wikidata...
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mStereum ostrea [3m [23m
#>  [32m■■■■■■■                          [39m  20% | ETA: 23s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mOssicaulis lachnopus [3m [23m
#>  [32m■■■■■■■                          [39m  20% | ETA: 23s [36mℹ [39m Getting page views from Wikipedia for  [3m [3mStereum hirsutum [3m [23m
#>  [32m■■■■■■■                          [39m  20% | ETA: 23s [32m■■■■■■■■■■■■■                    [39m  40% | ETA: 33s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mBasidiodendron eyrei [3m [23m
#>  [32m■■■■■■■■■■■■■                    [39m  40% | ETA: 33s [32m■■■■■■■■■■■■■■■■                 [39m  50% | ETA: 24s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mSistotrema oblongisporum [3m [23m
#>  [32m■■■■■■■■■■■■■■■■                 [39m  50% | ETA: 24s [32m■■■■■■■■■■■■■■■■■■■              [39m  60% | ETA: 18s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mFomes fomentarius [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■              [39m  60% | ETA: 18s [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  70% | ETA: 23s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mMycena renatii [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  70% | ETA: 23s [36mℹ [39m Getting page views from Wikipedia for  [3m [3mCerocorticium molare [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  70% | ETA: 23s [36mℹ [39m Getting page views from Wikipedia for  [3m [3mAporpium canescens [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  70% | ETA: 23s [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m 100% | ETA:  0s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mHypochnicium analogum [3m [23m
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mStereum ostrea [3m [23m
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mOssicaulis lachnopus [3m [23m
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mStereum hirsutum [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■                       [39m  30% |  ETA: 20s
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mBasidiodendron eyrei [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■                       [39m  30% |  ETA: 20sFetching OpenAlex  [32m■■■■■■■■■■■■■                    [39m  40% |  ETA: 14s
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mSistotrema oblongisporum [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■■■■                    [39m  40% |  ETA: 14s [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mFomes fomentarius [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■■■■                    [39m  40% |  ETA: 14sFetching OpenAlex  [32m■■■■■■■■■■■■■■■■■■■              [39m  60% |  ETA: 14s
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mMycena renatii [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■■■■■■■■■■              [39m  60% |  ETA: 14s
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mCerocorticium molare [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■■■■■■■■■■              [39m  60% |  ETA: 14s
#>  [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mAporpium canescens [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■■■■■■■■■■              [39m  60% |  ETA: 14s [36mℹ [39m Fetching OpenAlex works for taxon:  [3m [3mHypochnicium analogum [3m [23m
#> Fetching OpenAlex  [32m■■■■■■■■■■■■■■■■■■■              [39m  60% |  ETA: 14s
```

×

**Warnings**

``` popup-pre
#> Warning in oa_request(oa_query(filter = filter_i, multiple_id = multiple_id, :
#> No records found!
#> Warning in oa_request(oa_query(filter = filter_i, multiple_id = multiple_id, :
#> No records found!
```

``` r
data_enriched <- data_clean |>
  # Add GBIF occurrence data (add_to_phyloseq defaults to TRUE for phyloseq objects)
  tax_gbif_occur_pq() |>
  # Add species interaction data from GLOBI
  tax_globi_pq(interaction_types = "hasHost") |>
  # Add Wikipedia information
  tax_get_wk_info_pq() |>
  # Add OpenAlex publication data
  tax_oa_pq()
```

``` r

print(paste("The enriched taxonomic table now has the following new columns: ", paste(colnames(data_enriched@tax_table)[!colnames(data_enriched@tax_table) %in% colnames(data_clean@tax_table)], collapse = ", ")))
```

    #> [1] "The enriched taxonomic table now has the following new columns:  Global_occurences, target_taxon_name, nb, hasHost, lang, page_length, page_views, taxon_id, n_doi, list_doi, n_citation, list_keywords"

🛈⚠

×

**Messages**

``` popup-pre
#>  [1m [22mScale for  [32msize [39m is already present.
#> Adding another scale for  [32msize [39m, which will replace the existing scale.
```

×

**Warnings**

``` popup-pre
#> Warning:  [1m [22mRemoved 9 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

``` r
# todo add a title and add openalex information on the plot
psm <- psmelt(data_enriched) |>
  mutate(nb_num = map_dbl(nb, ~ sum(as.numeric(unlist(strsplit(.x, "; "))), na.rm = TRUE))) |>
  mutate(Quercus_interaction = map_dbl(target_taxon_name, ~ grepl("Quercus", .x))) |>
  filter(!is.na(taxa_name) & taxa_name != "NA") |>
  group_by(taxa_name) |>
  summarise(
    Abundance = sum(Abundance),
    page_views = mean(as.numeric(page_views), na.rm = TRUE),
    Guild = unique(Guild),
    nb_num = mean(nb_num, na.rm = TRUE),
    n_doi = as.numeric(unique(n_doi)),
    Quercus_interaction = unique(Quercus_interaction)
  ) |>
  mutate(page_views = ifelse(is.na(page_views) | page_views == 0, NA, page_views)) |>
  mutate(n_doi = ifelse(is.na(n_doi), 0, n_doi)) |>
  mutate(taxa_name_italic = map_chr(taxa_name, ~ ifelse(length(strsplit(.x, " ")[[1]]) == 2,
    paste0("italic('", .x, "')"),
    .x
  )))
psm
```

    #> # A tibble: 15 × 8
    #>    taxa_name         Abundance page_views Guild nb_num n_doi Quercus_interaction
    #>    <chr>                 <dbl>      <dbl> <chr>  <dbl> <dbl>               <dbl>
    #>  1 Antrodiella           20584         NA Wood…      0     0                   0
    #>  2 Aporpium canesce…     10784         NA Unde…      0     1                   0
    #>  3 Auricularia            9895         NA Unde…      0     0                   0
    #>  4 Basidiodendron e…     19661         41 Unde…     92     7                   1
    #>  5 Cerocorticium mo…     10878         NA Unde…      0     0                   0
    #>  6 Fomes fomentarius     25336      32135 Wood…    462   475                   1
    #>  7 Helicogloea           11053         NA Unde…      0     0                   0
    #>  8 Hypochnicium ana…     10178         NA Unde…      0     1                   0
    #>  9 Mycena renatii        12922         NA Leaf…      0     0                   0
    #> 10 Ossicaulis lachn…     33792         48 Wood…      0     4                   0
    #> 11 Phanerochaete         10215         NA Wood…      0     0                   0
    #> 12 Sistotrema oblon…     16933         35 Ecto…     14     3                   1
    #> 13 Stereum hirsutum      20660       3190 Unde…    758   243                   1
    #> 14 Stereum ostrea        74225       1859 Unde…    466    35                   1
    #> 15 Xylodon               38237         NA Unde…      0     0                   0
    #> # ℹ 1 more variable: taxa_name_italic <chr>

``` r

ggplot(psm, aes(
  y = forcats::fct_reorder(taxa_name, Abundance),
  x = log10(1 + Abundance),
  size = n_doi,
  color = Guild,
  shape = Quercus_interaction == 0
)) +
  geom_point() +
  geom_text(aes(label = page_views), size = 2.5, color = "black", nudge_x = 0.07) +
  scale_size_continuous(name = "Number of publications") +
  xlab("Molecular abundance (log10 scale)") +
  ylab("Taxa") +
  scale_y_discrete(labels = parse(text = psm$taxa_name_italic)) +
  theme_idest(plot_title_size = 12, subtitle_size = 9, axis_text_family = "mono", axis_text_size = 8) +
  theme(
    legend.text = element_text(size = 8),
    legend.key.size = unit(1, "line")
  ) +
  scale_size(range = c(2, 6)) +
  labs(
    title = "Number of sequences for each taxa.",
    subtitle = stringr::str_wrap("Color of points indicates the ecological guild. Shape indicates if the taxon is hosted by Quercus species in GLOBI. The number on the right of each point indicates the number of wikipedia page views on the last 30 days.", width = 112)
  )
```

![Scatter plot showing molecular abundance (log10 scale) on the x-axis
and taxa names on the y-axis. Points are colored by ecological guild,
sized by number of publications, and shaped indicate the interaction
with Quercus species. Wikipedia page view counts are displayed as text
labels next to each point.](figures/unnamed-chunk-6-1.png)

## Using Taxnames Instead of Phyloseq Objects

Most functions in **taxinfo** can work with either a phyloseq object or
a vector of taxonomic names. This is useful when you want to query
information for specific taxa without having a phyloseq object:

🛈

×

**Messages**

``` popup-pre
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mAmanita muscaria [3m [23m
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mBoletus edulis [3m [23m
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mCantharellus cibarius [3m [23m
#>  [36mℹ [39m Getting taxonomic IDs from Wikidata...
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mAmanita muscaria [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA: 24s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mBoletus edulis [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA: 24s [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m 100% | ETA:  0s
#>  [36mℹ [39m Getting page views from Wikipedia for  [3m [3mCantharellus cibarius [3m [23m
```

``` r
# Using taxnames parameter - returns a tibble
taxa_to_query <- c("Amanita muscaria", "Boletus edulis", "Cantharellus cibarius")

# Get GBIF occurrence data for specific taxa
gbif_data <- tax_gbif_occur_pq(taxnames = taxa_to_query)
```

``` r
head(gbif_data)
```

    #> # A tibble: 3 × 2
    #>   Global_occurences canonicalName        
    #>               <int> <chr>                
    #> 1            271186 Amanita muscaria     
    #> 2             69053 Boletus edulis       
    #> 3             66228 Cantharellus cibarius

``` r

# Get Wikipedia information
wiki_data <- tax_get_wk_info_pq(taxnames = taxa_to_query)
```

``` r
head(wiki_data)
```

    #> # A tibble: 3 × 5
    #>    lang page_length page_views taxon_id taxa_name            
    #>   <int>       <dbl>      <int> <chr>    <chr>                
    #> 1    82      13832.     285985 Q131227  Amanita muscaria     
    #> 2    81      13897.     104566 Q19740   Boletus edulis       
    #> 3    66       6684.      67843 Q188749  Cantharellus cibarius

``` r

# When using taxnames, add_to_phyloseq is automatically set to FALSE
# and the function returns a tibble instead of a phyloseq object
```

**Key Points:** - When using `taxnames`, the `add_to_phyloseq` parameter
is automatically set to `FALSE` and functions return tibbles - When
using a phyloseq object, `add_to_phyloseq` defaults to `TRUE` and
returns an enriched phyloseq object - You cannot use both `physeq` and
`taxnames` at the same time - The `add_to_phyloseq` parameter cannot be
`TRUE` when using `taxnames`

### Step 4: Add Custom Database Information

You can also integrate custom databases or trait information. Here we
will add fungal traits from a CSV file.

🛈

×

**Messages**

``` popup-pre
#>  [32m✔ [39m Added  [34m [34m18 [34m [39m columns from  [34m [34m/tmp/RtmpBEsapu/temp_libpath79c6fe8cd81/taxinfo/extdata/fun_trait_mini.csv [34m [39m with information for  [34m [34m0 [34m [39m taxa in the tax_table slot of the phyloseq object
```

``` r
fungal_traits <- system.file("extdata",
  "fun_trait_mini.csv",
  package = "taxinfo"
)

data_final <- tax_info_pq(data_enriched,
  taxonomic_rank = "genusEpithet",
  file_name = fungal_traits,
  csv_taxonomic_rank = "GENUS",
  col_prefix = "ft_",
  sep = ";"
)
```

``` r

dim(data_final)
```

    #> NULL

## Function Categories

### Taxonomic Name Standardization

- [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md):
  Verify and standardize taxonomic names using Global Names Architecture
  through the `taxize` package.

### Biodiversity Data Integration

- [`tax_gbif_occur_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_occur_pq.md):
  Retrieve GBIF occurrence data
- [`tax_globi_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_globi_pq.md):
  Access species interaction data from GLOBI
- [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md):
  Add information from CSV files

### Knowledge Base Integration

- [`tax_get_wk_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_info_pq.md):
  Get comprehensive Wikipedia data
- [`tax_oa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_oa_pq.md):
  Retrieve scientific literature from OpenAlex

### Geographic Analysis

- [`range_bioreg_pq()`](https://adrientaudiere.github.io/taxinfo/reference/range_bioreg_pq.md):
  Analyze biogeographic ranges
- [`plot_tax_gbif_pq()`](https://adrientaudiere.github.io/taxinfo/reference/plot_tax_gbif_pq.md):
  Create distribution maps

### Check for credibility and validity of the presence of taxonomic names (`species`)

- [`tax_check_ecoregion()`](https://adrientaudiere.github.io/taxinfo/reference/tax_check_ecoregion.md):
  Validate occurrences against ecoregions
- [`tax_retroblast_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_retroblast_pq.md):
  Sequence-based taxonomic verification
- [`tax_photos_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_photos_pq.md):
  Access taxonomic images and media
- [`tax_occur_check_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_check_pq.md):
  Multi-source occurrence validation

## Best Practices

1.  **Use conservative way to identify taxa at the species level**, most
    taxinfo function rely directly on species-level identification
2.  **Always start with name verification** using
    [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md)
3.  **Use appropriate data sources** for your taxonomic group of
    interest

## Next Steps

- **Checking Taxa Presence**: Learn how to validate if taxa are likely
  present in your samples
- **GBIF-based Functions**: Explore occurrence data and distribution
  mapping  
- **Adding External Information**: Integrate Wikipedia, GLOBI, and
  custom databases

This provides the foundation for using taxinfo effectively. Each
subsequent vignette will dive deeper into specific functionality areas.

## Session information

``` r
sessionInfo()
```

    #> R version 4.5.1 (2025-06-13)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Kali GNU/Linux Rolling
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.29.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=fr_FR.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=fr_FR.UTF-8        LC_COLLATE=fr_FR.UTF-8    
    #>  [5] LC_MONETARY=fr_FR.UTF-8    LC_MESSAGES=fr_FR.UTF-8   
    #>  [7] LC_PAPER=fr_FR.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=fr_FR.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> time zone: Europe/Paris
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] taxinfo_0.1.2      MiscMetabar_0.14.4 purrr_1.2.0        dplyr_1.1.4       
    #> [5] dada2_1.38.0       Rcpp_1.1.0         ggplot2_4.0.1      phyloseq_1.54.0   
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] RColorBrewer_1.1-3          rstudioapi_0.17.1          
    #>   [3] jsonlite_2.0.0              magrittr_2.0.4             
    #>   [5] farver_2.1.2                rmarkdown_2.30             
    #>   [7] fs_1.6.6                    ragg_1.5.0                 
    #>   [9] vctrs_0.6.5                 multtest_2.66.0            
    #>  [11] Rsamtools_2.26.0            RCurl_1.98-1.17            
    #>  [13] forcats_1.0.1               progress_1.2.3             
    #>  [15] htmltools_0.5.8.1           S4Arrays_1.10.0            
    #>  [17] curl_7.0.0                  Rhdf5lib_1.32.0            
    #>  [19] SparseArray_1.10.1          rhdf5_2.54.0               
    #>  [21] sass_0.4.10                 bslib_0.9.0                
    #>  [23] htmlwidgets_1.6.4           desc_1.4.3                 
    #>  [25] plyr_1.8.9                  zoo_1.8-14                 
    #>  [27] cachem_1.1.0                GenomicAlignments_1.46.0   
    #>  [29] whisker_0.4.1               igraph_2.2.1               
    #>  [31] lifecycle_1.0.4             iterators_1.0.14           
    #>  [33] pkgconfig_2.0.3             Matrix_1.7-4               
    #>  [35] R6_2.6.1                    fastmap_1.2.0              
    #>  [37] MatrixGenerics_1.22.0       digest_0.6.38              
    #>  [39] ShortRead_1.68.0            S4Vectors_0.48.0           
    #>  [41] textshaping_1.0.4           GenomicRanges_1.62.0       
    #>  [43] hwriter_1.3.2.1             vegan_2.7-2                
    #>  [45] labeling_0.4.3              urltools_1.7.3.1           
    #>  [47] httr_1.4.7                  abind_1.4-8                
    #>  [49] mgcv_1.9-4                  compiler_4.5.1             
    #>  [51] bit64_4.6.0-1               withr_3.0.2                
    #>  [53] S7_0.2.1                    BiocParallel_1.44.0        
    #>  [55] WikidataQueryServiceR_1.0.0 MASS_7.3-65                
    #>  [57] DelayedArray_0.36.0         biomformat_1.38.0          
    #>  [59] permute_0.9-8               oai_0.4.0                  
    #>  [61] tools_4.5.1                 ape_5.8-1                  
    #>  [63] rgbif_3.8.4                 glue_1.8.0                 
    #>  [65] nlme_3.1-168                rhdf5filters_1.22.0        
    #>  [67] grid_4.5.1                  WikidataR_2.3.3            
    #>  [69] cluster_2.1.8.1             reshape2_1.4.5             
    #>  [71] ade4_1.7-23                 generics_0.1.4             
    #>  [73] gtable_0.3.6                ratelimitr_0.4.2           
    #>  [75] tzdb_0.5.0                  tidyr_1.3.1                
    #>  [77] data.table_1.17.8           hms_1.1.4                  
    #>  [79] utf8_1.2.6                  xml2_1.5.0                 
    #>  [81] XVector_0.50.0              BiocGenerics_0.56.0        
    #>  [83] foreach_1.5.2               pillar_1.11.1              
    #>  [85] stringr_1.6.0               vroom_1.6.6                
    #>  [87] rglobi_0.3.4                splines_4.5.1              
    #>  [89] lattice_0.22-7              survival_3.8-3             
    #>  [91] bit_4.6.0                   deldir_2.0-4               
    #>  [93] tidyselect_1.2.1            pbapply_1.7-4              
    #>  [95] Biostrings_2.78.0           knitr_1.50                 
    #>  [97] IRanges_2.44.0              Seqinfo_1.0.0              
    #>  [99] SummarizedExperiment_1.40.0 crul_1.6.0                 
    #> [101] stats4_4.5.1                xfun_0.54                  
    #> [103] wikitaxa_0.4.0              Biobase_2.70.0             
    #> [105] taxize_0.10.0               matrixStats_1.5.0          
    #> [107] stringi_1.8.7               lazyeval_0.2.2             
    #> [109] yaml_2.3.10                 evaluate_1.0.5             
    #> [111] codetools_0.2-20            cigarillo_1.0.0            
    #> [113] httpcode_0.3.0              interp_1.1-6               
    #> [115] tibble_3.3.0                cli_3.6.5                  
    #> [117] RcppParallel_5.1.11-1       systemfonts_1.3.1          
    #> [119] jquerylib_0.1.4             triebeard_0.4.1            
    #> [121] tidyverse_2.0.0             png_0.1-8                  
    #> [123] parallel_4.5.1              assertthat_0.2.1           
    #> [125] pkgdown_2.2.0               readr_2.1.6                
    #> [127] openalexR_2.0.2             prettyunits_1.2.0          
    #> [129] latticeExtra_0.6-31         jpeg_0.1-11                
    #> [131] WikipediR_1.7.1             bitops_1.0-9               
    #> [133] pwalign_1.6.0               scales_1.4.0               
    #> [135] crayon_1.5.3                rlang_1.1.6
