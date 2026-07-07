# Getting Started with taxinfo: Augment phyloseq Objects with Taxonomic Information

🛈

×

**Messages**

``` popup-pre
#> Loading required package: MiscMetabar
#> Loading required package: phyloseq
#> Loading required package: ggplot2
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
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

| Source         | Description                                                                | Key Functions                                                                                                                                                                                    |
|----------------|----------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **GBIF**       | Global biodiversity occurrence data                                        | [`tax_gbif_occur_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_gbif_occur_pq.md), [`plot_tax_gbif_pq()`](https://adrientaudiere.github.io/taxinfo/reference/plot_tax_gbif_pq.md) |
| **Wikipedia**  | Encyclopedia data and page statistics                                      | [`tax_get_wk_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_info_pq.md)                                                                                               |
| **GLOBI**      | Species interaction networks                                               | [`tax_globi_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_globi_pq.md)                                                                                                           |
| **OpenAlex**   | Scientific literature database                                             | [`tax_oa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_oa_pq.md)                                                                                                                 |
| **GNA**        | Global Names Architecture for name verification                            | [`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md)                                                                                                     |
| **Custom CSV** | Any database in CSV format with a column documenting Taxonomic information | [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md)                                                                                                             |

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
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 20
#> • Taxa submitted for verification: 19
#> • Genus-level only taxa: 2
#> • Total matches found: 15
#> • Synonyms: 4 (including 4 at genus level)
#> • Accepted names: 11 (including 6 at genus level)
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

    #> Taxonomy Table:     [6 taxa by 22 taxonomic ranks]:
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
    #>       genusEpithet specificEpithet genusSpeciesEpithet    namePublishedInYear
    #> ASV7  NA           NA              NA                     NA                 
    #> ASV8  "Stereum"    "ostrea"        "Stereum ostrea"       "1838"             
    #> ASV12 "Xylodon"    NA              NA                     "1821"             
    #> ASV18 "Stereum"    "ostrea"        "Stereum ostrea"       "1838"             
    #> ASV25 "Ossicaulis" "lachnopus"     "Ossicaulis lachnopus" "2000"             
    #> ASV26 "Stereum"    "hirsutum"      "Stereum hirsutum"     "1800"             
    #>       authorship bracketauthorship scientificNameAuthorship
    #> ASV7  NA         NA                NA                      
    #> ASV8  "Fr."      "Blume & T.Nees"  "(Blume & T.Nees) Fr."  
    #> ASV12 "Gray"     "Pers."           "(Pers.) Gray"          
    #> ASV18 "Fr."      "Blume & T.Nees"  "(Blume & T.Nees) Fr."  
    #> ASV25 "Contu"    "Fr."             "(Fr.) Contu"           
    #> ASV26 "Pers."    "Willd."          "(Willd.) Pers."

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
#> ✔ GNA verification summary:
#> • Taxa submitted for verification: 26
#> • Total matches found: 0
#> • Synonyms: 0 (including 0 at genus level)
#> • Accepted names: 0 (including 0 at genus level)
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
    #> Columns: 11
    #> $ submittedName            <chr> "Stereum ostrea", "Xylodon raduloides", "Ossi…
    #> $ currentName              <chr> "Stereum ostrea (Blume & T.Nees) Fr., 1838", …
    #> $ currentCanonicalSimple   <chr> "Stereum ostrea", "Xylodon", "Ossicaulis lach…
    #> $ genusEpithet             <chr> "Stereum", "Xylodon", "Ossicaulis", "Stereum"…
    #> $ specificEpithet          <chr> "ostrea", NA, "lachnopus", "hirsutum", NA, "e…
    #> $ genusSpeciesEpithet      <chr> "Stereum ostrea", NA, "Ossicaulis lachnopus",…
    #> $ namePublishedInYear      <chr> "1838", "1821", "2000", "1800", "1980", "1963…
    #> $ authorship               <chr> "Fr.", "Gray", "Contu", "Pers.", "Ryvarden & …
    #> $ bracketauthorship        <chr> "Blume & T.Nees", "Pers.", "Fr.", "Willd.", N…
    #> $ scientificNameAuthorship <chr> "(Blume & T.Nees) Fr.", "(Pers.) Gray", "(Fr.…
    #> $ taxa_names_in_phyloseq   <chr> "Stereum ostrea", "Xylodon raduloides", "Ossi…

### Step 3: Add Biodiversity Information

Once names are verified, you can enrich your data with various sources:

🛈

×

**Messages**

``` popup-pre
#> ℹ Processing GBIF occurrences for Stereum ostrea
#> ℹ Processing GBIF occurrences for Ossicaulis lachnopus
#> 
■■■■■■■■■■■                       33% | ETA:  3s [K

 [Kℹ Processing GBIF occurrences for Stereum hirsutum
#> ■■■■■■■■■■■                       33% | ETA:  3s

■■■■■■■■■■■■■■                    44% | ETA:  2s [K

 [Kℹ Processing GBIF occurrences for Basidiodendron eyrei
#> ■■■■■■■■■■■■■■                    44% | ETA:  2s

■■■■■■■■■■■■■■■■■■                56% | ETA:  2s [K

 [Kℹ Processing GBIF occurrences for Sistotrema oblongisporum
#> ■■■■■■■■■■■■■■■■■■                56% | ETA:  2s

■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  1s [K

 [Kℹ Processing GBIF occurrences for Fomes fomentarius
#> ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  1s

■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  1s [K

 [Kℹ Processing GBIF occurrences for Cerocorticium molare
#> ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  1s

■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  0s [K

 [Kℹ Processing GBIF occurrences for Aporpium canescens
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  0s

 [Kℹ Processing GBIF occurrences for Hypochnicium analogum
#> ℹ Getting taxonomic IDs from Wikidata...
#> ℹ Getting page views from Wikipedia for Stereum ostrea
#> 
■■■■■■■                           20% | ETA: 23s [K

 [Kℹ Getting page views from Wikipedia for Ossicaulis lachnopus
#> ■■■■■■■                           20% | ETA: 23s

■■■■■■■■■■                        30% | ETA: 20s [K

 [Kℹ Getting page views from Wikipedia for Stereum hirsutum
#> ■■■■■■■■■■                        30% | ETA: 20s

■■■■■■■■■■■■■                     40% | ETA: 34s [K

 [Kℹ Getting page views from Wikipedia for Basidiodendron eyrei
#> ■■■■■■■■■■■■■                     40% | ETA: 34s

■■■■■■■■■■■■■■■■                  50% | ETA: 25s [K

 [Kℹ Getting page views from Wikipedia for Sistotrema oblongisporum
#> ■■■■■■■■■■■■■■■■                  50% | ETA: 25s

■■■■■■■■■■■■■■■■■■■               60% | ETA: 18s [K

 [Kℹ Getting page views from Wikipedia for Fomes fomentarius
#> ■■■■■■■■■■■■■■■■■■■               60% | ETA: 18s

■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 24s [K

 [Kℹ Getting page views from Wikipedia for Mycena renatii
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 24s

 [Kℹ Getting page views from Wikipedia for Cerocorticium molare
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 24s

 [Kℹ Getting page views from Wikipedia for Aporpium canescens
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 24s

 [Kℹ Getting page views from Wikipedia for Hypochnicium analogum
#> ℹ Fetching OpenAlex works for taxon: Stereum ostrea
#> HTTP status 429 Too Many Requestsℹ Fetching OpenAlex works for taxon: Ossicaulis lachnopus
#> HTTP status 429 Too Many Requestsℹ Fetching OpenAlex works for taxon: Stereum hirsutum
#> HTTP status 429 Too Many Requestsℹ Fetching OpenAlex works for taxon: Basidiodendron eyrei
#> HTTP status 429 Too Many Requestsℹ Fetching OpenAlex works for taxon: Sistotrema oblongisporum
#> HTTP status 429 Too Many Requests
Fetching OpenAlex ■■■■■■■■■■■■■■■■                  50% |  ETA:  1s [K

 [Kℹ Fetching OpenAlex works for taxon: Fomes fomentarius
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■                  50% |  ETA:  1s
HTTP status 429 Too Many Requests
 [Kℹ Fetching OpenAlex works for taxon: Mycena renatii
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■                  50% |  ETA:  1s
HTTP status 429 Too Many Requests
 [Kℹ Fetching OpenAlex works for taxon: Cerocorticium molare
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■                  50% |  ETA:  1s
HTTP status 429 Too Many Requests
 [Kℹ Fetching OpenAlex works for taxon: Aporpium canescens
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■                  50% |  ETA:  1s
HTTP status 429 Too Many Requests
 [Kℹ Fetching OpenAlex works for taxon: Hypochnicium analogum
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■                  50% |  ETA:  1s
HTTP status 429 Too Many Requests
 [K
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

    #> Error in `filter()`:
    #> ℹ In argument: `type %in% type_works`.
    #> Caused by error:
    #> ! object 'type' not found

``` r

print(paste("The enriched taxonomic table now has the following new columns: ", paste(colnames(data_enriched@tax_table)[!colnames(data_enriched@tax_table) %in% colnames(data_clean@tax_table)], collapse = ", ")))
```

    #> Error:
    #> ! object 'data_enriched' not found

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
```

    #> Error:
    #> ! object 'data_enriched' not found

``` r
psm
```

    #> Error:
    #> ! object 'psm' not found

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

    #> Error:
    #> ! object 'psm' not found

## Using Taxnames Instead of Phyloseq Objects

Most functions in **taxinfo** can work with either a phyloseq object or
a vector of taxonomic names. This is useful when you want to query
information for specific taxa without having a phyloseq object:

🛈

×

**Messages**

``` popup-pre
#> ℹ Processing GBIF occurrences for Amanita muscaria
#> ℹ Processing GBIF occurrences for Boletus edulis
#> ℹ Processing GBIF occurrences for Cantharellus cibarius
#> ℹ Getting taxonomic IDs from Wikidata...
#> ℹ Getting page views from Wikipedia for Amanita muscaria
#> 
■■■■■■■■■■■■■■■■■■■■■             67% | ETA: 27s [K

 [Kℹ Getting page views from Wikipedia for Boletus edulis
#> ■■■■■■■■■■■■■■■■■■■■■             67% | ETA: 27s

 [Kℹ Getting page views from Wikipedia for Cantharellus cibarius
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
    #> 1            316020 Amanita muscaria     
    #> 2             79350 Boletus edulis       
    #> 3             73690 Cantharellus cibarius

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
    #> 1    83      21259.      12239 Q131227  Amanita muscaria     
    #> 2    82      14549.      24154 Q19740   Boletus edulis       
    #> 3    66       6946.       7876 Q188749  Cantharellus cibarius

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

    #> Error:
    #> ! object 'data_enriched' not found

``` r

dim(data_final)
```

    #> Error:
    #> ! object 'data_final' not found

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

    #> R version 4.6.0 (2026-04-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Pop!_OS 24.04 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    #>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    #>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> time zone: Europe/Paris
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] taxinfo_0.1.2      MiscMetabar_0.16.8 dplyr_1.2.1        ggplot2_4.0.3     
    #> [5] phyloseq_1.56.0   
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] ade4_1.7-24           tidyselect_1.2.1      farver_2.1.2         
    #>  [4] urltools_1.7.3.1      Biostrings_2.80.1     S7_0.2.2             
    #>  [7] bitops_1.0-9          divent_0.5-4          RCurl_1.98-1.19      
    #> [10] lazyeval_0.2.3        WikipediR_1.7.1       digest_0.6.39        
    #> [13] wikitaxa_0.5.0        lifecycle_1.0.5       cluster_2.1.8.2      
    #> [16] survival_3.8-6        magrittr_2.0.5        compiler_4.6.0       
    #> [19] rlang_1.2.0           tools_4.6.0           utf8_1.2.6           
    #> [22] igraph_2.3.2          data.table_1.18.4     knitr_1.51           
    #> [25] bit_4.6.0             curl_7.1.0            plyr_1.8.9           
    #> [28] xml2_1.5.2            RColorBrewer_1.1-3    httpcode_0.3.0       
    #> [31] withr_3.0.2           purrr_1.2.2           BiocGenerics_0.58.1  
    #> [34] triebeard_0.4.1       grid_4.6.0            stats4_4.6.0         
    #> [37] multtest_2.68.0       biomformat_1.40.0     scales_1.4.0         
    #> [40] iterators_1.0.14      MASS_7.3-65           crul_1.6.0           
    #> [43] taxize_0.10.1         cli_3.6.6             vegan_2.7-5          
    #> [46] crayon_1.5.3          generics_0.1.4        otel_0.2.0           
    #> [49] RcppParallel_5.1.11-2 tzdb_0.5.0            httr_1.4.8           
    #> [52] rgbif_3.8.5           reshape2_1.4.5        ape_5.8-1            
    #> [55] stringr_1.6.0         splines_4.6.0         parallel_4.6.0       
    #> [58] XVector_0.52.0        vctrs_0.7.3           Matrix_1.7-5         
    #> [61] jsonlite_2.0.0        hms_1.1.4             IRanges_2.46.0       
    #> [64] S4Vectors_0.50.1      bit64_4.8.2           foreach_1.5.2        
    #> [67] rglobi_0.3.4          tidyr_1.3.2           glue_1.8.1           
    #> [70] codetools_0.2-20      stringi_1.8.7         gtable_0.3.6         
    #> [73] tibble_3.3.1          pillar_1.11.1         Seqinfo_1.2.0        
    #> [76] R6_2.6.1              Rdpack_2.6.6          vroom_1.7.1          
    #> [79] evaluate_1.0.5        oai_0.4.0             lattice_0.22-9       
    #> [82] Biobase_2.72.0        readr_2.2.0           rbibutils_2.4.1      
    #> [85] openalexR_3.0.1       Rcpp_1.1.1-1.1        nlme_3.1-169         
    #> [88] permute_0.9-10        whisker_0.4.1         mgcv_1.9-4           
    #> [91] xfun_0.58             zoo_1.8-15            pkgconfig_2.0.3
