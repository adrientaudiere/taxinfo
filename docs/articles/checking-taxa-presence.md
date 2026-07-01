# Checking Taxa Presence in Samples

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
#> Loading required package: divent
```

``` r
library(taxinfo)
```

``` r
library(MiscMetabar)
library(ggplot2)
library(dplyr)
```

## Overview

One of the key challenges in environmental sequencing studies is
determining whether detected taxa are likely to be actually present at
your sampling sites. This vignette demonstrates how to use `taxinfo`’s
occurrence checking functions to validate the likelihood of taxa
presence based on known geographic distributions.

As most of `taxinfo`’s utilities, these functions can accept either a
phyloseq object with cleaned taxonomic names or a vector of taxonomic
names (`taxnames` parameter). The algorithm used to assign the taxonomic
names must be stringent enough to avoid false positives. Moreover, we
recommend using
[`gna_verifier_pq()`](https://adrientaudiere.github.io/taxinfo/reference/gna_verifier_pq.md)
to clean taxonomic names by disambiguating synonyms and correcting
misspellings.

## Core Functions

The main functions for checking taxa presence are:

- [`tax_occur_check()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_check.md):
  Core occurrence checking function for individual taxa
- [`tax_occur_check_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_check_pq.md):
  Check occurrence likelihood within a radius around samples
- [`tax_occur_multi_check_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_occur_multi_check_pq.md):
  Check multiple locations simultaneously

## Basic Occurrence Checking for individual taxa

How many *Quercus robur* occurrences have been reported within 100km of
Paris, France?

🛈

×

**Messages**

``` popup-pre
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m957 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m95.7 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m755 [39m occurrences for species  [3mQuercus robur [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m7.82 [39m km
```

``` r
Q_rob_in_Paris <- tax_occur_check("Quercus robur", 2.3522, 48.8566, 100)
```

``` r
Q_rob_in_Paris$count_in_radius
```

    #> [1] 755

``` r
# Visualize occurrences around Paris for Fagus sylvatica
res_occ <- tax_occur_check("Fagus sylvatica", 2.3522, 48.8566, 200,
  return_all_occ = TRUE
)

occ_data_sf <- sf::st_as_sf(res_occ$occ_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

if (requireNamespace("leaflet")) {
  library(leaflet)
}
if (requireNamespace("leafpop")) {
  library(leafpop)
}
leaflet() |>
  addTiles() |>
  setView(2.3522, 48.8566, zoom = 12) |>
  fitBounds(
    lat1 = as.vector(sf::st_bbox(occ_data_sf))[2],
    lng1 = as.vector(sf::st_bbox(occ_data_sf))[1],
    lat2 = as.vector(sf::st_bbox(occ_data_sf))[4],
    lng2 = as.vector(sf::st_bbox(occ_data_sf))[3]
  ) |>
  leaflet::addCircles(data = occ_data_sf, color = "blue", stroke = 1, opacity = 0.8) |>
  leaflet::addCircleMarkers(2.3522, 48.8566, color = "orange", radius = 2, opacity = 1)
```

## Basic Occurrence Checking fo phyloseq object

### Setting Up the Data

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
#> Cleaning suppress 0 taxa and 6 samples.
```

``` r
# Load example data
data("data_fungi_mini", package = "MiscMetabar")

# Keep only first 20 taxa for speed
data_clean <- prune_taxa(taxa = taxa_names(data_fungi_mini)[1:20], data_fungi_mini) |>
  gna_verifier_pq(data_sources = 210)
```

``` r

summary_plot_pq(data_clean)
```

![Summary of a phyloseq object](figures/unnamed-chunk-4-1.png)

``` r
head(data_clean@tax_table[, c("Genus", "Species", "currentCanonicalSimple")])
```

    #> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
    #>       Genus        Species      currentCanonicalSimple
    #> ASV7  NA           NA           NA                    
    #> ASV8  "Stereum"    "ostrea"     "Stereum ostrea"      
    #> ASV12 "Xylodon"    "raduloides" "Xylodon"             
    #> ASV18 "Stereum"    "ostrea"     "Stereum ostrea"      
    #> ASV25 "Ossicaulis" "lachnopus"  "Ossicaulis lachnopus"
    #> ASV26 "Stereum"    "hirsutum"   "Stereum hirsutum"

### Single Location Occurrence Check

Check if taxa in your phyloseq object have been reported within a
specific radius of your sampling location:

🛈

×

**Messages**

``` popup-pre
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m47.07 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m477 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m490 [34m [39m
#> - Retention rate:  [34m [34m97.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m438 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.3 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m3 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m3 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m55.36 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m53 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m642 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m664 [34m [39m
#> - Retention rate:  [34m [34m96.7 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m599 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.17 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m28 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m30 [34m [39m
#> - Retention rate:  [34m [34m93.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m28 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m12.71 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m5 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m5 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m22 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m22 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
```

``` r
# Define sampling coordinates (example: Paris, France)
longitude <- 2.3488
latitude <- 48.8534

# Check occurrence within 100km radius
occurrence_check <- tax_occur_check_pq(
  data_clean,
  longitude = longitude,
  latitude = latitude,
  radius_km = 100,
  n_occur = 1000,
  add_to_phyloseq = FALSE
)
```

``` r

# View results
head(occurrence_check)
```

    #> # A tibble: 6 × 10
    #>   taxa_name                count_in_radius closest_distance_km mean_distance_km
    #>   <chr>                              <dbl>               <dbl>            <dbl>
    #> 1 Stereum ostrea                         1               47.1              47.1
    #> 2 Ossicaulis lachnopus                   0               NA                NA  
    #> 3 Stereum hirsutum                     438                8.3              57.3
    #> 4 Basidiodendron eyrei                   3               55.4              58.9
    #> 5 Sistotrema oblongisporum               1               53                53  
    #> 6 Fomes fomentarius                    599                8.17             54.2
    #> # ℹ 6 more variables: total_count_in_world <dbl>, search_radius <dbl>,
    #> #   closest_point_lat <dbl>, closest_point_lon <dbl>, sample_point_lat <dbl>,
    #> #   sample_point_lon <dbl>

### Example Visualization

Here’s what occurrence checking results might look like:

![Horizontal bar chart showing GBIF occurrence counts for taxa within a
100km radius around a sampling location. Bars are colored by genus and
show the ratio of local occurrences (within radius) to total global
occurrences. Taxa names are ordered by count in radius from lowest to
highest.](figures/unnamed-chunk-7-1.png)

We can also explore the minimum and mean distance to sampling location.

``` r
occurrence_check |>
  mutate(Genus = stringr::word(taxa_name, 1)) |>
  filter(!is.na(mean_distance_km)) |>
  mutate(taxa_name = forcats::fct_reorder(taxa_name, mean_distance_km)) |>
  ggplot(aes(x = mean_distance_km, y = taxa_name)) +
  geom_col(aes(fill = Genus)) +
  scale_fill_idest_d(name = "Genus") +
  geom_text(aes(label = paste0("Closest: ", closest_distance_km, " km"), x = 10), size = 3) +
  geom_text(aes(label = paste0("n=", count_in_radius)), nudge_x = -2, size = 3)
```

![Horizontal bar chart showing mean distance to sampling location for
each taxon. Bars are colored by genus and ordered by mean distance. Text
labels show the closest distance in kilometers and the count of
occurrences for each taxon.](figures/unnamed-chunk-8-1.png)

The results include:

- `taxa_name`: Taxonomic name checked
- `count_in_radius`: Number of occurrences within the specified radius
- `total_count_in_world`: Total global occurrences for this taxon
- `radius_km`: Search radius used
- `longitude`/`latitude`: Center coordinates
- `closest_distance_km`: The distance to the closest occurrence point
- `closest_distance_km`: The mean distance to occurrence point

## Advanced Occurrence Checking

### Multiple Radius Analysis

Compare occurrence patterns at different spatial scales:

🛈

×

**Messages**

``` popup-pre
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m47.07 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m272 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m285 [34m [39m
#> - Retention rate:  [34m [34m95.4 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m176 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.3 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m0 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m55.36 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m0 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m53 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m442 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m464 [34m [39m
#> - Retention rate:  [34m [34m95.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m217 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.17 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m24 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m24 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m17 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m12.71 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m5 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m5 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m0 [39m occurrences for species  [3mAporpium canescens [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m18 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m18 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m0 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m47.07 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m477 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m490 [34m [39m
#> - Retention rate:  [34m [34m97.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m438 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.3 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m3 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m3 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m55.36 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m53 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m642 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m664 [34m [39m
#> - Retention rate:  [34m [34m96.7 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m599 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.17 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m28 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m30 [34m [39m
#> - Retention rate:  [34m [34m93.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m28 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m12.71 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m5 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m5 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m22 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m22 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m47.07 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m992 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m99.2 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m690 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m9.51 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m6 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m6 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m6 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m55.36 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m10 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m10 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m7 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m53 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m978 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m97.8 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m750 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.17 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m64 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m64 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m47 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m12.71 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m5 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m5 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m22 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m22 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m12 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m15 [34m [39m
#> - Retention rate:  [34m [34m80 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m8 [39m occurrences for species  [3mStereum ostrea [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m47.07 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m953 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m95.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m798 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m39.08 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m255 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m264 [34m [39m
#> - Retention rate:  [34m [34m96.6 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m199 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m55.36 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m242 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m378 [34m [39m
#> - Retention rate:  [34m [34m64 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m199 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m53 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m953 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m95.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m834 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m37.76 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m906 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m90.6 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m906 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m103.21 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m11 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m12 [34m [39m
#> - Retention rate:  [34m [34m91.7 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m11 [39m occurrences for species  [3mAporpium canescens [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m22 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m28 [34m [39m
#> - Retention rate:  [34m [34m78.6 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
```

``` r
# Check multiple radii
radii <- c(50, 100, 200, 500)

occurrence_multi_radius <- map_dfr(radii, function(r) {
  tax_occur_check_pq(data_clean,
    longitude = longitude,
    latitude = latitude,
    radius_km = r,
    n_occur = 1000,
    add_to_phyloseq = FALSE
  ) |>
    mutate(radius_category = paste(r, "km"))
})
```

``` r

occurrence_multi_radius$radius_category <- factor(occurrence_multi_radius$radius_category,
  levels = paste(radii, "km")
)
```

``` r
# Visualize scaling patterns
occurrence_multi_radius |>
  ggplot(aes(x = radius_category, y = count_in_radius, group = taxa_name)) +
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.8) +
  facet_wrap(~taxa_name, scales = "free_y") +
  labs(
    title = "Occurrence Scaling by Radius",
    x = "Search Radius",
    y = "Count in Radius"
  ) +
  theme_idest(base_size = 8, strip_text_size = 7, strip_text_face = "italic")
```

![Small multiples faceted plot showing occurrence scaling patterns
across different search radii (50, 100, 200, 500 km) for each taxon.
Each panel represents one taxon with lines and points showing how
occurrence counts increase with larger search
radii.](figures/unnamed-chunk-10-1.png)

### Filtering Unlikely Taxa

Use occurrence data to filter taxa that are unlikely to be present:

🛈⚠

×

**Messages**

``` popup-pre
#> Number of non-matching ASV 0
#> Number of matching ASV 20
#> Number of filtered-out ASV 14
#> Number of kept ASV 6
#> Number of kept samples 137
#> Cleaning suppress 0 taxa and 110 samples.
#> Compute the number of clusters
#> Compute the number of samples
```

×

**Warnings**

``` popup-pre
#> Warning in verify_pq(physeq, verbose = verbose): At least one of your sample
#> contains less than 500 sequences.
#> Warning in verify_pq(physeq, verbose = verbose): At least one of your taxa is
#> represent by less than 1 sequences.
#> Warning in verify_pq(physeq, verbose = verbose): At least one of your samples
#> metadata columns contains NA.
```

``` r
# Set threshold for likely presence (e.g., at least 5 occurrences within 100km)
min_occurrences <- 5

likely_present <- occurrence_check |>
  filter(count_in_radius >= min_occurrences) |>
  pull(taxa_name)

# Filter phyloseq object to keep only likely present taxa
# Note that all samples are kept, use clean_pq if you want to remove empty samples
data_filtered <- select_taxa_pq(data_clean, taxnames = likely_present)
```

``` r

compar <- MiscMetabar::track_wkflow(
  list(
    "initial" = data_clean,
    "filtered" = clean_pq(data_filtered)
  ),
  verbose = FALSE
)
```

``` r
knitr::kable(compar)
```

    <!-- KNITR_ASIS_OUTPUT_TOKEN -->

    |         | nb_sequences| nb_clusters| nb_samples|
    |:--------|------------:|-----------:|----------:|
    |initial  |       388567|          20|        137|
    |filtered |        77836|           6|         27|

    <!-- KNITR_ASIS_OUTPUT_TOKEN -->```
    </div>


    ## Multi-Location Checking

    For studies with multiple sampling sites:

    <div class='chunk-with-popups' style='position: relative; margin: 0; padding: 0;'>
    <div class='popup-icons' style='position: absolute; bottom: 5px; right: 5px; z-index: 10; background: rgba(255,255,255,0.9); padding: 2px 6px; border-radius: 4px;'><span class='popup-toggle' data-target='msg-popup-unnamed-chunk-12' title='Messages' aria-haspopup='true' aria-expanded='false'>🛈</span></div>
    <div id='msg-popup-unnamed-chunk-12' class='popup' role='dialog' aria-hidden='true' onclick='event.stopPropagation();'><button class='popup-close' aria-label='Close'>&times;</button><div class='popup-header'><strong>Messages</strong></div><pre class='popup-pre'>#&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.71999129665565_47.942620956242" [34m [39m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [36m• [39m Closest occurrence:  [34m68.45 [39m km
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m348 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m360 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.7 [34m [39m%
    #&gt; 
    #&gt;  [32m✔ [39m Found  [34m301 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [36m• [39m Closest occurrence:  [34m16.12 [39m km
    #&gt; 
    #&gt;  [32m■                                [39m   1% | ETA: 11m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.05106341096905_48.1789923251039" [34m [39m
    #&gt; 
    #&gt;  [32m■                                [39m   1% | ETA: 11m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■                                [39m   1% | ETA: 11m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■                                [39m   1% | ETA: 11m
    #&gt;  [36m• [39m Closest occurrence:  [34m32.85 [39m km
    #&gt; 
    #&gt;  [32m■                                [39m   1% | ETA: 11m
    #&gt;  [32m■■                               [39m   2% | ETA: 14m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.51254727775609_48.0134608879384" [34m [39m
    #&gt; 
    #&gt;  [32m■■                               [39m   2% | ETA: 14m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■                               [39m   2% | ETA: 14m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■                               [39m   2% | ETA: 14m
    #&gt;  [36m• [39m Closest occurrence:  [34m73.45 [39m km
    #&gt; 
    #&gt;  [32m■■                               [39m   2% | ETA: 14m
    #&gt;  [32m■■                               [39m   3% | ETA: 13m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.99888574265077_47.9674647325587" [34m [39m
    #&gt; 
    #&gt;  [32m■■                               [39m   3% | ETA: 13m
    #&gt; Cleaning suppress 14 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■                               [39m   3% | ETA: 13m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■                               [39m   3% | ETA: 13m
    #&gt;  [36m• [39m Closest occurrence:  [34m55.73 [39m km
    #&gt; 
    #&gt;  [32m■■                               [39m   3% | ETA: 13m
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.12431054428304_47.8345379661378" [34m [39m
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [36m• [39m Closest occurrence:  [34m67.48 [39m km
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.22968232120521_48.3753011242051" [34m [39m
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [36m• [39m Closest occurrence:  [34m7.34 [39m km
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.63563646780467_48.1532880397758" [34m [39m
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [36m• [39m Closest occurrence:  [34m56.83 [39m km
    #&gt; 
    #&gt;  [32m■■                               [39m   4% | ETA: 11m
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.9505349395853_48.1959913391173" [34m [39m
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [36m• [39m Closest occurrence:  [34m35.8 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.95116007864432_48.2643561983257" [34m [39m
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [36m• [39m Closest occurrence:  [34m30.6 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.94345891023711_47.7760578342778" [34m [39m
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [36m• [39m Closest occurrence:  [34m77.14 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   6% | ETA:  8m
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.88926012326226_48.1029199637079" [34m [39m
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.1257964084072_47.6981800328461" [34m [39m
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m82.45 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m208 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m208 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m193 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m17.91 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m85.74 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m90.01 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m488 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m488 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m454 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m8.4 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m17 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m17 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m17 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m19.39 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m87.98 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m85.74 [39m km
    #&gt; 
    #&gt;  [32m■■■                              [39m   8% | ETA:  6m
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.41300497907184_48.3065482960645" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.67380211958355_48.0858294742264" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt;  [36m• [39m Closest occurrence:  [34m59.1 [39m km
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m4 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m4 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt;  [32m✔ [39m Found  [34m4 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt;  [36m• [39m Closest occurrence:  [34m82.66 [39m km
    #&gt; 
    #&gt;  [32m■■■■                             [39m   9% | ETA:  7m
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.10248538997036_48.0244206828945" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m47.23 [39m km
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.62739770158633_47.7723975198812" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m87.92 [39m km
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.89559749705091_47.8883969741163" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt; Cleaning suppress 14 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m66.86 [39m km
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m4 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m4 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m80.24 [39m km
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m80.11 [39m km
    #&gt; 
    #&gt;  [32m■■■■                             [39m  11% | ETA:  6m
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.98947961800922_48.2105077073803" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.10859926853223_48.1355367288603" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m35.35 [39m km
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m29 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m32 [34m [39m
    #&gt; - Retention rate:  [34m [34m90.6 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt;  [32m✔ [39m Found  [34m29 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt;  [36m• [39m Closest occurrence:  [34m44.16 [39m km
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  13% | ETA:  6m
    #&gt;  [32m■■■■■                            [39m  15% | ETA:  5m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.81718503454801_48.0076999094129" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  15% | ETA:  5m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  15% | ETA:  5m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  15% | ETA:  5m
    #&gt;  [36m• [39m Closest occurrence:  [34m58.33 [39m km
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  15% | ETA:  5m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.09363088409011_47.9287237625992" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  15% | ETA:  5m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■                            [39m  15% | ETA:  5m
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.072590251173_48.156568820368" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [36m• [39m Closest occurrence:  [34m34.28 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.73909129099304_48.1608823232258" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [36m• [39m Closest occurrence:  [34m50.07 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.14755526425101_47.6199878353286" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [36m• [39m Closest occurrence:  [34m90.89 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  16% | ETA:  5m
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.37770098584691_48.1871568572318" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m28.25 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m31.42 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.98051097911836_47.9381896993138" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m59.26 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■                           [39m  18% | ETA:  4m
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.81283052929986_48.0526133353925" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.99680993774989_47.6418816288284" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m90.39 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.83464220925331_47.8423482310774" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m73.45 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m290 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m294 [34m [39m
    #&gt; - Retention rate:  [34m [34m98.6 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m247 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m7.06 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■                          [39m  20% | ETA:  4m
    #&gt;  [32m■■■■■■■■                         [39m  22% | ETA:  4m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.69752006974744_47.7733956662559" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  22% | ETA:  4m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  22% | ETA:  4m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  22% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m0 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  22% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m101.8 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  22% | ETA:  4m
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.18707263796022_48.0727305136746" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m40.59 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.03529772211249_47.9428224171394" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m57.38 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m68.47 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  23% | ETA:  4m
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.04873709294924_48.1035338267995" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m40.36 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m57.71 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.32470977668872_47.9794182659762" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m50.51 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.02240761657395_47.8051860813469" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m72.32 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■                         [39m  24% | ETA:  4m
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.97320059738749_48.2541344601592" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m30.07 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m51.91 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.61798250645556_48.1921729574156" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m55.71 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m396 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m409 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m355 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m32.33 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  26% | ETA:  3m
    #&gt;  [32m■■■■■■■■■                        [39m  28% | ETA:  4m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.94415255168303_48.1537442739188" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  28% | ETA:  4m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  28% | ETA:  4m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  28% | ETA:  4m
    #&gt;  [36m• [39m Closest occurrence:  [34m39.68 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■                        [39m  28% | ETA:  4m
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.93731080434842_48.2071861542162" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m35.58 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.2134615757523_47.9052225851454" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m58.84 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.01400697005933_47.7449330250727" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m78.96 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m20 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m20 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m17 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m23.22 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  28% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.87217533522287_47.9388758652341" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m62.64 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.99000702019037_48.4423538973677" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.94970331134996_47.7916663238243" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m75.34 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m87.98 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■                       [39m  31% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.08895942315855_47.7706952299571" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.55108351506737_47.6649345393271" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m87.73 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  33% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■                      [39m  34% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.00930627608884_48.3051877310075" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  34% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m405 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m418 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.9 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  34% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m375 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  34% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m12.78 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■                      [39m  34% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.11554181386133_48.1108371029796" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.02363897488194_48.3986220529725" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m19.21 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m429 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m442 [34m [39m
    #&gt; - Retention rate:  [34m [34m97.1 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m395 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m3.13 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m28 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m30 [34m [39m
    #&gt; - Retention rate:  [34m [34m93.3 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m28 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m21.26 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  35% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■                     [39m  36% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.61765590179436_47.9691758520888" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  36% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  36% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  36% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m71.11 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  36% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  36% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.17241729636771_48.5128816675995" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m39.32 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.9513526520733_48.2123998289092" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m34.44 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■                     [39m  37% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.95878256104368_48.2285389756858" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m32.79 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.00383551844183_48.2247677686525" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m394 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m407 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m349 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m19.12 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m53.99 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  39% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.00591215086395_47.9205997013881" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt; Cleaning suppress 14 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m60.42 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m320 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m329 [34m [39m
    #&gt; - Retention rate:  [34m [34m97.3 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m302 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m1.01 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m71.76 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m499 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m99.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m459 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m2.64 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  40% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.10996550836034_47.8353477698866" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m67.59 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.54517702862141_47.8842230750576" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.53651143671127_48.3527578755143" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m20.99 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m429 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m442 [34m [39m
    #&gt; - Retention rate:  [34m [34m97.1 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m352 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m1.8 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m12.23 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m12.19 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m479 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m95.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m423 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m3.34 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m28 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m30 [34m [39m
    #&gt; - Retention rate:  [34m [34m93.3 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m28 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m10.61 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m9.94 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m9.79 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■                    [39m  41% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.92775574890126_48.0265984292808" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.04267114994201_48.0752998656957" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt; Cleaning suppress 14 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m43.41 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m348 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m376 [34m [39m
    #&gt; - Retention rate:  [34m [34m92.6 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m322 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m7.64 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  43% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.21486917645194_48.2277415306806" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m23.27 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.8669823502909_48.2482526150614" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m36.7 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.22279048379384_48.1224181889918" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m34.74 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■                   [39m  45% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.95082071766417_47.9141239825768" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.76448733821134_48.2720922653541" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m41.99 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.80482987676803_47.9858285138646" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m60.84 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.21301146400543_47.9455692631358" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m54.38 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.02633412695458_47.5106639941574" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mStereum ostrea [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.0977257618402_48.013097328282" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m48.54 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  49% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.66010988643086_47.7802982196348" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.70585273872451_47.873364364895" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m75.33 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.05683006872538_47.5872691098922" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.26746408264712_48.5297864057125" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.0473392566573_47.7693203227088" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.26365867680693_47.9318724247938" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [36m• [39m Closest occurrence:  [34m55.69 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■                 [39m  51% | ETA:  3m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.10478195750024_48.157272515262" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m33.22 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.12134960931142_47.7458973779576" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.97801286557103_48.1084283098884" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m42.37 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m361 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m386 [34m [39m
    #&gt; - Retention rate:  [34m [34m93.5 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m326 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m13.44 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m61.16 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m61.6 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m479 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m95.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m419 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m12.75 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m29 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m32 [34m [39m
    #&gt; - Retention rate:  [34m [34m90.6 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m29 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m50.04 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m59.35 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m59.16 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  55% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.0344363430822_48.0150211800336" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m49.84 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m481 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.2 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m433 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m7.64 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■               [39m  58% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.98193454259987_48.1117028843329" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m41.91 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.38486866827156_48.0830812797844" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m39.65 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.25967855176618_47.7095400462268" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.14975825350737_48.1882412243403" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.11124486582086_47.932212825658" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m328 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m336 [34m [39m
    #&gt; - Retention rate:  [34m [34m97.6 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m309 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m5.54 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m497 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m99.4 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m468 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m8.38 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■              [39m  58% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  62% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.89034854725223_47.9848851505099" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  62% | ETA:  2m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m483 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.6 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  62% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m416 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  62% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m0.74 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  62% | ETA:  2m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  62% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.22210697851637_48.0080408784101" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m47.4 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.47753313342314_48.0248602132255" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.96886124475399_47.8003134898826" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  63% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.08677795798345_48.2466780129764" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.92360977763677_48.0680848975123" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m48.32 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.08483751490949_47.9054595035764" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m60.36 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m319 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m323 [34m [39m
    #&gt; - Retention rate:  [34m [34m98.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m303 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m5.92 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■             [39m  65% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.21262039916976_48.1417506121062" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m32.71 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.20974252395115_47.6942082570681" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m82.27 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.99237942103737_48.0474850690346" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m47.79 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m63.71 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■            [39m  67% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.09722978390305_47.7374371507058" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m78.47 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.3345765221665_48.1494057174173" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt;  [36m• [39m Closest occurrence:  [34m31.76 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  69% | ETA:  2m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.92912776728871_47.6874963130708" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m86.83 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m17 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m17 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m17 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m18.4 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.18926957729331_48.0142106719136" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m47 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■           [39m  71% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.26336527120883_47.8720930459586" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m62.34 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m63.87 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.94067199508163_47.8309608522896" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m71.45 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m270 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m273 [34m [39m
    #&gt; - Retention rate:  [34m [34m98.9 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m243 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m2.34 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m82.24 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m84.94 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m499 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m99.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m453 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m5.98 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m26 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m26 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m25 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m33.42 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m82.74 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m82.24 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  72% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.92255728495032_48.1350489396163" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m42.31 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.84291346884393_48.230675158785" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m39.28 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m4 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m4 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m4 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m64.98 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.78865262660137_47.6626990515116" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m93.04 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m17 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m17 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m4 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m22.87 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■          [39m  74% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.84089171409044_47.819437010156" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.64874491436582_48.2635267395648" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m50.22 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.86189242063565_48.220037948909" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.88829160115624_48.2407535678774" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m35.9 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m408 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m421 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.9 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m358 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m23.11 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m4 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m4 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m4 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m61.43 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m60.72 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m479 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m95.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m432 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m24.11 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m28 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m30 [34m [39m
    #&gt; - Retention rate:  [34m [34m93.3 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m28 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m40.84 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m58.59 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m58.33 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■         [39m  76% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.89266733470054_47.713745844622" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m84.93 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.04542542654565_48.2765821721141" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m24.47 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.19569098404865_48.0006251880824" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m48.43 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  79% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  81% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.95822346972963_47.9844226351225" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  81% | ETA:  1m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m337 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m362 [34m [39m
    #&gt; - Retention rate:  [34m [34m93.1 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  81% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m304 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  81% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m5.36 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■        [39m  81% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.72011790799765_48.0882856451857" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m56.36 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.0517074575817_48.0257845792447" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m48.27 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m335 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m361 [34m [39m
    #&gt; - Retention rate:  [34m [34m92.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m321 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m7.54 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m481 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.2 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m445 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m7.54 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  82% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.91164010941773_47.8339571479273" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m71.93 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.11371997224307_47.8992814180577" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m60.56 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.42537009180682_47.761271763598" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.08497168826285_47.8496553354218" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [36m• [39m Closest occurrence:  [34m66.4 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■       [39m  83% | ETA:  1m
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.66314369355006_48.2911682806016" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [36m• [39m Closest occurrence:  [34m48.13 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [36m• [39m Closest occurrence:  [34m75.27 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.04988035679387_47.8342792934421" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [36m• [39m Closest occurrence:  [34m68.67 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  86% | ETA: 47s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.21456765036328_48.0579548920752" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m41.93 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m351 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m373 [34m [39m
    #&gt; - Retention rate:  [34m [34m94.1 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m333 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m5.41 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m49.98 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m52.48 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m479 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m95.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m450 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m5.41 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m29 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m32 [34m [39m
    #&gt; - Retention rate:  [34m [34m90.6 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m29 [39m occurrences for species  [3mCerocorticium molare [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m45.44 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m50.29 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m22 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m22 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [36m• [39m Closest occurrence:  [34m49.98 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 42s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.40787385250057_47.9039893031458" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt;  [36m• [39m Closest occurrence:  [34m59.55 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m339 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m339 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt;  [32m✔ [39m Found  [34m290 [39m occurrences for species  [3mStereum hirsutum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt;  [36m• [39m Closest occurrence:  [34m5.23 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  88% | ETA: 41s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  89% | ETA: 38s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.08989075561208_47.879034129157" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  89% | ETA: 38s
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No taxonomic names found at the specified taxonomic rank.Please check the `taxonomic_rank` parameter and your phyloseq object.
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  89% | ETA: 38s
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.27836280912767_48.2920220360792" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  89% | ETA: 38s
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  89% | ETA: 38s
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  89% | ETA: 38s
    #&gt;  [36m• [39m Closest occurrence:  [34m32.36 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  89% | ETA: 38s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.08531330938071_48.0299358708191" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [36m• [39m Closest occurrence:  [34m47.02 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [36m• [39m Closest occurrence:  [34m58.96 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.02151679844273_47.7133357799404" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.00445894662567_47.9979393362743" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m481 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m96.2 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [32m✔ [39m Found  [34m434 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [36m• [39m Closest occurrence:  [34m5.16 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  91% | ETA: 33s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.12072220210142_47.9575527930324" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt;  [36m• [39m Closest occurrence:  [34m54.11 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m486 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m97.2 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt;  [32m✔ [39m Found  [34m441 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt;  [36m• [39m Closest occurrence:  [34m9.87 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 25s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.94746988543012_47.8187319642872" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt;  [36m• [39m Closest occurrence:  [34m72.56 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m499 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m500 [34m [39m
    #&gt; - Retention rate:  [34m [34m99.8 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt;  [32m✔ [39m Found  [34m450 [39m occurrences for species  [3mFomes fomentarius [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt;  [36m• [39m Closest occurrence:  [34m7.43 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  93% | ETA: 23s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.8943471835258_47.5795695042209" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt;  [36m• [39m Closest occurrence:  [34m99.07 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m5 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m5 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt;  [32m✔ [39m Found  [34m0 [39m occurrences for species  [3mAporpium canescens [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt;  [36m• [39m Closest occurrence:  [34m107.79 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  94% | ETA: 21s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.03842988440752_48.3786720927342" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.77076006620141_47.8063748325952" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt;  [36m• [39m Closest occurrence:  [34m79.19 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.16923693295953_47.9794793927246" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt; Cleaning suppress 19 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt;  [36m• [39m Closest occurrence:  [34m51.04 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    [39m  95% | ETA: 18s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.01634392585031_48.0479919144244" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt; Cleaning suppress 16 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [36m• [39m Closest occurrence:  [34m46.98 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [36m• [39m Closest occurrence:  [34m63.52 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.73897659801294_48.01217977865" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m4 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m4 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [32m✔ [39m Found  [34m4 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [36m• [39m Closest occurrence:  [34m81.87 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [36m• [39m Closest occurrence:  [34m82.3 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  97% | ETA: 10s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.81101758801005_47.5644847943449" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt; Cleaning suppress 15 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [32m✔ [39m Found  [34m0 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [36m• [39m Closest occurrence:  [34m102.62 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"2.09086831886303_47.9764279713501" [34m [39m
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt; Cleaning suppress 18 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m3 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m3 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [36m• [39m Closest occurrence:  [34m62.89 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [36m• [39m Closest occurrence:  [34m65.33 [39m km
    #&gt; 
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m  99% | ETA:  5s
    #&gt;  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m 100% | ETA:  0s
    #&gt; 
    #&gt;  [36mℹ [39m Processing GPS point:  [34m [34m"1.82895949983833_48.0224589574812" [34m [39m
    #&gt; 
    #&gt; Cleaning suppress 17 taxa and 0 samples.
    #&gt; 
    #&gt;  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
    #&gt;  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
    #&gt; -  [34m [34m1 [34m [39m occurrences remain(s)
    #&gt; - Total original:  [34m [34m1 [34m [39m
    #&gt; - Retention rate:  [34m [34m100 [34m [39m%
    #&gt; 
    #&gt;  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
    #&gt; 
    #&gt;  [36m• [39m Closest occurrence:  [34m56.49 [39m km
    #&gt; 
    #&gt; Cleaning suppress 10 taxa ( ASV7 / ASV12 / ASV25 / ASV27 / ASV34 / ASV41 / ASV42 / ASV46 / ASV49 / ASV54 ) and 36 sample(s) ( AD26-005-B_S9_MERGED.fastq.gz / AD26-005-H_S10_MERGED.fastq.gz / AD26-005-M_S11_MERGED.fastq.gz / ADABM30X-M_S16_MERGED.fastq.gz / B18-006-B_S19_MERGED.fastq.gz / BG7-010-H_S31_MERGED.fastq.gz / BJ8-ABM-003_S35_MERGED.fastq.gz / BQ4-018-M_S51_MERGED.fastq.gz / BR8-005_S53_MERGED.fastq.gz / BT7-006_S56_MERGED.fastq.gz / CB8-019-B_S69_MERGED.fastq.gz / CB8-019-H_S70_MERGED.fastq.gz / CB8-019-M_S71_MERGED.fastq.gz / D18-003-M_S80_MERGED.fastq.gz / D9-027-M_S85_MERGED.fastq.gz / DJ2-008-H_S88_MERGED.fastq.gz / DS1-ABM002-B_S91_MERGED.fastq.gz / DS1-ABM002-H_S92_MERGED.fastq.gz / DS1-ABM002-M_S93_MERGED.fastq.gz / DY5-004-B_S96_MERGED.fastq.gz / DY5-004-H_S97_MERGED.fastq.gz / F6-ABM-001_S105_MERGED.fastq.gz / F7-015-M_S106_MERGED.fastq.gz / H24-NVABM1-H_S111_MERGED.fastq.gz / J18-004-B_S114_MERGED.fastq.gz / J18-004-M_S116_MERGED.fastq.gz / N23-002-B_S130_MERGED.fastq.gz / NVABM-0163-H_S135_MERGED.fastq.gz / NVABM0216_S136_MERGED.fastq.gz / NVABM0244-M_S137_MERGED.fastq.gz / P27-ABM001_S155_MERGED.fastq.gz / T28-ABM602-B_S162_MERGED.fastq.gz / W25-ABMX_S164_MERGED.fastq.gz / W9-025-M_S169_MERGED.fastq.gz / X29-004-B_S174_MERGED.fastq.gz / Y31-ABM484-B_S184_MERGED.fastq.gz ).
    #&gt; 
    #&gt;  [36mℹ [39m After filtering taxa with at least  [34m [34m1 [34m [39m GBIF occurrences within  [34m [34m100 [34m [39mkm:/n  - Taxa:  [34m [34m10 [34m [39m/ [34m [34m20 [34m [39m remain/n  - Samples:  [34m [34m101 [34m [39m/ [34m [34m137 [34m [39m remain/n  - Occurrences:  [34m [34m175 [34m [39m/ [34m [34m357 [34m [39m remain
    </pre></div>


    ``` r
    sample_locations <- data.frame(
      longitude = rnorm(nsamples(data_clean), mean = 2, sd = 0.2),
      latitude = rnorm(nsamples(data_clean), mean = 48, sd = 0.2)
    )

    # Check occurrences for multiple locations
    multi_location_check <- tax_occur_multi_check_pq(
      data_clean,
      longitudes = sample_locations$longitude,
      latitude = sample_locations$latitude,
      radius_km = 100,
      n_occur = 500
    )

``` r

# Summarize by site
site_summary <- multi_location_check[[1]] |>
  group_by(sample_name) |>
  summarise(
    taxa_with_occurrences = sum(count_in_radius > 0),
    mean_nb_occurrences = mean(count_in_radius),
    .groups = "drop"
  ) |>
  arrange(desc(taxa_with_occurrences), desc(mean_nb_occurrences))

site_summary
```

    #> # A tibble: 134 × 3
    #>    sample_name                       taxa_with_occurrences mean_nb_occurrences
    #>    <chr>                                             <int>               <dbl>
    #>  1 NVABM0244-M_S137_MERGED.fastq.gz                      8               94.6 
    #>  2 T28-ABM602-B_S162_MERGED.fastq.gz                     8               93.8 
    #>  3 CB8-019-H_S70_MERGED.fastq.gz                         8               92.8 
    #>  4 DY5-004-H_S97_MERGED.fastq.gz                         8               89.6 
    #>  5 N23-002-B_S130_MERGED.fastq.gz                        8               83.7 
    #>  6 AD26-005-H_S10_MERGED.fastq.gz                        8               77.3 
    #>  7 C9-005_S65_MERGED.fastq.gz                            4              191.  
    #>  8 O9-005-B_S152_MERGED.fastq.gz                         3              256.  
    #>  9 BV11-002-B_S57_MERGED.fastq.gz                        3              141.  
    #> 10 ADABM30X-H_S15_MERGED.fastq.gz                        3                2.25
    #> # ℹ 124 more rows

### Sample Site Validation

Validate your sampling sites against known distributions:

🛈

×

**Messages**

``` popup-pre
#>  [33m! [39m No occurrences found for  [3m [3mStereum ostrea [3m [23m
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m185 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m198 [34m [39m
#> - Retention rate:  [34m [34m93.4 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m164 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m11 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mBasidiodendron eyrei [3m [23m
#> 
#>  [33m! [39m No occurrences found for  [3m [3mSistotrema oblongisporum [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m249 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m271 [34m [39m
#> - Retention rate:  [34m [34m91.9 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m208 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m9.28 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m17 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m17 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m16 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m16.25 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mAporpium canescens [3m [23m
#> 
#>  [33m! [39m No occurrences found for  [3m [3mHypochnicium analogum [3m [23m
#> 
#>  [33m! [39m No occurrences found for  [3m [3mStereum ostrea [3m [23m
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m980 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m98 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m783 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m7.43 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mBasidiodendron eyrei [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m7 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m7 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m6 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m17.83 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m948 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1000 [34m [39m
#> - Retention rate:  [34m [34m94.8 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m678 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m20.43 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m241 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m249 [34m [39m
#> - Retention rate:  [34m [34m96.8 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m184 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m16.56 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mAporpium canescens [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m2 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m2 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m2 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m32.1 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mStereum ostrea [3m [23m
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m10 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m12 [34m [39m
#> - Retention rate:  [34m [34m83.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m10 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m3.78 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mBasidiodendron eyrei [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m11.84 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m7 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m7 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m6 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.03 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mCerocorticium molare [3m [23m
#> 
#>  [33m! [39m No occurrences found for  [3m [3mAporpium canescens [3m [23m
#> 
#>  [33m! [39m No occurrences found for  [3m [3mHypochnicium analogum [3m [23m
```

``` r
# Define your sampling coordinates
sample_coords <- data.frame(
  longitude = c(2.3, 5.4, -1.6), # Example coordinates
  latitude = c(48.9, 50.6, 47.2)
)

# Check if sampling sites overlap with known ranges
site_validation <- map_dfr(1:nrow(sample_coords), function(i) {
  tax_occur_check_pq(data_clean,
    longitude = sample_coords$longitude[i],
    latitude = sample_coords$latitude[i],
    radius_km = 50,
    add_to_phyloseq = FALSE
  )
})
```

``` r

# Summarize validation results
validation_summary <- site_validation |>
  group_by(sample_point_lon, sample_point_lat) |>
  summarise(
    taxa_with_occurrences = sum(count_in_radius > 0),
    mean_nb_occurrences = mean(count_in_radius),
    .groups = "drop"
  )

validation_summary
```

    #> # A tibble: 3 × 4
    #>   sample_point_lon sample_point_lat taxa_with_occurrences mean_nb_occurrences
    #>              <dbl>            <dbl>                 <int>               <dbl>
    #> 1             -1.6             47.2                     3                1.89
    #> 2              2.3             48.9                     3               43.1 
    #> 3              5.4             50.6                     5              184.

## Integration with Phyloseq

Add occurrence information directly to your phyloseq object:

🛈

×

**Messages**

``` popup-pre
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m47.07 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m477 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m490 [34m [39m
#> - Retention rate:  [34m [34m97.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m438 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.3 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m3 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m3 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m55.36 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m53 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m642 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m664 [34m [39m
#> - Retention rate:  [34m [34m96.7 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m599 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m8.17 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m28 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m30 [34m [39m
#> - Retention rate:  [34m [34m93.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m28 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m12.71 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m5 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m5 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m22 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m22 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m52.86 [39m km
```

``` r
# Add occurrence data to phyloseq tax_table
data_with_occurrence <- tax_occur_check_pq(data_clean,
  longitude = longitude,
  latitude = latitude,
  radius_km = 100
)
```

``` r

# View enhanced tax_table with occurrence columns
head(data_with_occurrence@tax_table)
```

    #> Taxonomy Table:     [6 taxa by 30 taxonomic ranks]:
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
    #> ASV7  "NA_NA"                "NA"                  
    #> ASV8  "Stereum_ostrea"       "Stereum ostrea"      
    #> ASV12 "Xylodon_raduloides"   "Xylodon"             
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
    #>       bracketauthorship scientificNameAuthorship count_in_radius
    #> ASV7  NA                NA                       NA             
    #> ASV8  "Blume & T.Nees"  "(Blume & T.Nees) Fr."   "  1"          
    #> ASV12 "Pers."           "(Pers.) Gray"           NA             
    #> ASV18 "Blume & T.Nees"  "(Blume & T.Nees) Fr."   "  1"          
    #> ASV25 "Fr."             "(Fr.) Contu"            "  0"          
    #> ASV26 "Willd."          "(Willd.) Pers."         "438"          
    #>       closest_distance_km mean_distance_km total_count_in_world search_radius
    #> ASV7  NA                  NA               NA                   NA           
    #> ASV8  "47.07"             "47.07"          "  7331"             "100"        
    #> ASV12 NA                  NA               NA                   NA           
    #> ASV18 "47.07"             "47.07"          "  7331"             "100"        
    #> ASV25 NA                  NA               "     0"             "100"        
    #> ASV26 " 8.30"             "57.27"          "114395"             "100"        
    #>       closest_point_lat closest_point_lon sample_point_lat sample_point_lon
    #> ASV7  NA                NA                NA               NA              
    #> ASV8  "48.43261"        "2.278890"        "48.8534"        "2.3488"        
    #> ASV12 NA                NA                NA               NA              
    #> ASV18 "48.43261"        "2.278890"        "48.8534"        "2.3488"        
    #> ASV25 NA                NA                "48.8534"        "2.3488"        
    #> ASV26 "48.79612"        "2.421410"        "48.8534"        "2.3488"

## Interpreting Results

### Occurrence Patterns

- **High local, high global**: Cosmopolitan species, expected presence
- **High local, low global**: Regionally endemic species
- **Low local, high global**: Cosmopolitan but locally rare
- **Low local, low global**: Rare species or potential errors

### Quality Control Applications

Use occurrence checking for:

1.  **Contamination detection**: Taxa with zero local occurrences may be
    contaminants
2.  **Sample validation**: Ensure detected communities match
    biogeographic expectations
3.  **Filtering decisions**: Remove unlikely taxa from downstream
    analyses
4.  **Hypothesis generation**: Identify interesting biogeographic
    patterns

### Best Practices

1.  **Use appropriate radii** for your study system and organism
    dispersal capability
2.  **Consider temporal factors** - GBIF data spans many years
3.  **Account for sampling bias** in GBIF data (some regions/taxa better
    sampled)
4.  **Validate results** with ecological knowledge of your study system
5.  **Document thresholds** used for filtering decisions

## Integration with Other Functions

Occurrence checking works well with other taxinfo functions:

🛈⚠

×

**Messages**

``` popup-pre
#>  [32m✔ [39m GNA verification summary:
#>  [36m• [39m Total taxa in phyloseq:  [34m45 [39m
#>  [36m• [39m Taxa submitted for verification:  [34m37 [39m
#>  [36m• [39m Genus-level only taxa:  [34m2 [39m
#>  [36m• [39m Total matches found:  [34m25 [39m
#>  [36m• [39m Synonyms:  [34m4 [39m (including  [34m4 [39m at genus level)
#>  [36m• [39m Accepted names:  [34m21 [39m (including  [34m15 [39m at genus level)
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mStereum ostrea [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m40.88 [39m km
#> 
#>  [33m! [39m No occurrences found for  [3m [3mOssicaulis lachnopus [3m [23m
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m483 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m496 [34m [39m
#> - Retention rate:  [34m [34m97.4 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m442 [39m occurrences for species  [3mStereum hirsutum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m5.83 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m3 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m3 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m3 [39m occurrences for species  [3mBasidiodendron eyrei [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m51.95 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m1 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m1 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m1 [39m occurrences for species  [3mSistotrema oblongisporum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m49.56 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m645 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m667 [34m [39m
#> - Retention rate:  [34m [34m96.7 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m605 [39m occurrences for species  [3mFomes fomentarius [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m6.53 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m28 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m30 [34m [39m
#> - Retention rate:  [34m [34m93.3 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m28 [39m occurrences for species  [3mCerocorticium molare [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m5.78 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m5 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m5 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m5 [39m occurrences for species  [3mAporpium canescens [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m49.4 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m22 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m22 [34m [39m
#> - Retention rate:  [34m [34m100 [34m [39m%
#> 
#>  [32m✔ [39m Found  [34m22 [39m occurrences for species  [3mHypochnicium analogum [23m:
#> 
#>  [36m• [39m Closest occurrence:  [34m49.55 [39m km
#> 
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m0 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m8 [34m [39m
#> - Retention rate:  [34m [34m0 [34m [39m%
#>  [33m! [39m No valid occurrences for  [3m [3mHyphoderma roseocremeum [3m [23m
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth... [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m37 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m40 [34m [39m
#> - Retention rate:  [34m [34m92.5 [34m [39m%
#>  [32m✔ [39m Found  [34m37 [39m occurrences for species  [3mHyphoderma setigerum [23m:
#>  [36m• [39m Closest occurrence:  [34m41.07 [39m km
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth... [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m747 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m780 [34m [39m
#> - Retention rate:  [34m [34m95.8 [34m [39m%
#>  [32m✔ [39m Found  [34m675 [39m occurrences for species  [3mTrametes versicolor [23m:
#>  [36m• [39m Closest occurrence:  [34m5.8 [39m km
#>  [33m! [39m No occurrences found for  [3m [3mPeniophora versiformis [3m [23m
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth... [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m143 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m145 [34m [39m
#> - Retention rate:  [34m [34m98.6 [34m [39m%
#>  [32m✔ [39m Found  [34m126 [39m occurrences for species  [3mExidia glandulosa [23m:
#>  [36m• [39m Closest occurrence:  [34m12.22 [39m km
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth... [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m39 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m44 [34m [39m
#> - Retention rate:  [34m [34m88.6 [34m [39m%
#>  [32m✔ [39m Found  [34m39 [39m occurrences for species  [3mPeniophorella pubera [23m:
#>  [36m• [39m Closest occurrence:  [34m27.23 [39m km
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth... [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m233 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m247 [34m [39m
#> - Retention rate:  [34m [34m94.3 [34m [39m%
#>  [32m✔ [39m Found  [34m212 [39m occurrences for species  [3mAuricularia mesenterica [23m:
#>  [36m• [39m Closest occurrence:  [34m3.28 [39m km
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth... [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m48 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m54 [34m [39m
#> - Retention rate:  [34m [34m88.9 [34m [39m%
#>  [32m✔ [39m Found  [34m48 [39m occurrences for species  [3mHericium coralloides [23m:
#>  [36m• [39m Closest occurrence:  [34m31.14 [39m km
#>  [1m [22mReading  [34mne_50m_land.zip [39m from naturalearth...
#>  [36mℹ [39m After cleaning with CoordinateCleaner::clean_coordinates:
#> -  [34m [34m51 [34m [39m occurrences remain(s)
#> - Total original:  [34m [34m53 [34m [39m
#> - Retention rate:  [34m [34m96.2 [34m [39m%
#>  [32m✔ [39m Found  [34m51 [39m occurrences for species  [3mXylodon flaviporus [23m:
#>  [36m• [39m Closest occurrence:  [34m40.88 [39m km
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mStereum ostrea [3m [23m
#>  [32m■■■■■                            [39m  13% | ETA:  7s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mStereum hirsutum [3m [23m
#>  [32m■■■■■                            [39m  13% | ETA:  7s [32m■■■■■■■                          [39m  20% | ETA:  7s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mBasidiodendron eyrei [3m [23m
#>  [32m■■■■■■■                          [39m  20% | ETA:  7s [36mℹ [39m Processing GBIF occurrences for  [3m [3mSistotrema oblongisporum [3m [23m
#>  [32m■■■■■■■                          [39m  20% | ETA:  7s [36mℹ [39m Processing GBIF occurrences for  [3m [3mFomes fomentarius [3m [23m
#>  [32m■■■■■■■                          [39m  20% | ETA:  7s [36mℹ [39m Processing GBIF occurrences for  [3m [3mCerocorticium molare [3m [23m
#>  [32m■■■■■■■                          [39m  20% | ETA:  7s [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  5s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mAporpium canescens [3m [23m
#>  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  5s [36mℹ [39m Processing GBIF occurrences for  [3m [3mHypochnicium analogum [3m [23m
#>  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  5s [36mℹ [39m Processing GBIF occurrences for  [3m [3mHyphoderma setigerum [3m [23m
#>  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  5s [36mℹ [39m Processing GBIF occurrences for  [3m [3mTrametes versicolor [3m [23m
#>  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  5s [36mℹ [39m Processing GBIF occurrences for  [3m [3mExidia glandulosa [3m [23m
#>  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  5s [36mℹ [39m Processing GBIF occurrences for  [3m [3mPeniophorella pubera [3m [23m
#>  [32m■■■■■■■■■■■■■■■                  [39m  47% | ETA:  5s [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  87% | ETA:  1s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mAuricularia mesenterica [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  87% | ETA:  1s [36mℹ [39m Processing GBIF occurrences for  [3m [3mHericium coralloides [3m [23m
#>  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■      [39m  87% | ETA:  1s [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  [39m 100% | ETA:  0s
#>  [36mℹ [39m Processing GBIF occurrences for  [3m [3mXylodon flaviporus [3m [23m
#> Cleaning suppress 0 taxa and 27 samples.
```

×

**Warnings**

``` popup-pre
#> Warning in cc_outl(otl_test, lon = lon, lat = lat, species = species, method =
#> outliers_method, : Species with fewer than 7 unique records will not be tested.
#> Warning in cc_outl(otl_test, lon = lon, lat = lat, species = species, method =
#> outliers_method, : Species with fewer than 7 unique records will not be tested.
```

``` r
data_complete <- data_fungi_mini |>
  # 1. Clean names
  gna_verifier_pq(data_sources = 210) |>
  # 2. Check occurrences
  tax_occur_check_pq(
    longitude = 2.3, latitude = 48.8,
    radius_km = 100
  ) |>
  subset_taxa(as.numeric(count_in_radius) > 0) |> # Keep only taxa known in a radius of 100km
  # 3. Add GBIF occurrence data
  tax_gbif_occur_pq() |>
  # 4. Filter based on frequency in samples (Keep taxa in >2 samples)
  filter_taxa(function(x) sum(x > 0) > 2, prune = TRUE) |>
  # 5. Remove empty samples and taxa
  clean_pq()
```

``` r
data_complete@tax_table[1:5, ]
```

    #> Taxonomy Table:     [5 taxa by 31 taxonomic ranks]:
    #>       Domain  Phylum          Class            Order           
    #> ASV8  "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"    
    #> ASV18 "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"    
    #> ASV26 "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"    
    #> ASV29 "Fungi" "Basidiomycota" "Agaricomycetes" "Auriculariales"
    #> ASV32 "Fungi" "Basidiomycota" "Agaricomycetes" "Cantharellales"
    #>       Family                              Genus            Species        
    #> ASV8  "Stereaceae"                        "Stereum"        "ostrea"       
    #> ASV18 "Stereaceae"                        "Stereum"        "ostrea"       
    #> ASV26 "Stereaceae"                        "Stereum"        "hirsutum"     
    #> ASV29 "Exidiaceae"                        "Basidiodendron" "eyrei"        
    #> ASV32 "Cantharellales_fam_Incertae_sedis" "Sistotrema"     "oblongisporum"
    #>       Trophic.Mode             Guild                             Trait      
    #> ASV8  "Saprotroph"             "Undefined Saprotroph"            "White Rot"
    #> ASV18 "Saprotroph"             "Undefined Saprotroph"            "White Rot"
    #> ASV26 "Saprotroph"             "Undefined Saprotroph"            "White Rot"
    #> ASV29 "Saprotroph"             "Undefined Saprotroph"            "NULL"     
    #> ASV32 "Saprotroph-Symbiotroph" "Ectomycorrhizal-Wood Saprotroph" "White Rot"
    #>       Confidence.Ranking Genus_species              taxa_name                 
    #> ASV8  "Probable"         "Stereum_ostrea"           "Stereum ostrea"          
    #> ASV18 "Probable"         "Stereum_ostrea"           "Stereum ostrea"          
    #> ASV26 "Probable"         "Stereum_hirsutum"         "Stereum hirsutum"        
    #> ASV29 "Probable"         "Basidiodendron_eyrei"     "Basidiodendron eyrei"    
    #> ASV32 "Possible"         "Sistotrema_oblongisporum" "Sistotrema oblongisporum"
    #>       currentName                                             
    #> ASV8  "Stereum ostrea (Blume & T.Nees) Fr., 1838"             
    #> ASV18 "Stereum ostrea (Blume & T.Nees) Fr., 1838"             
    #> ASV26 "Stereum hirsutum (Willd.) Pers., 1800"                 
    #> ASV29 "Basidiodendron eyrei (Wakef.) Luck-Allen, 1963"        
    #> ASV32 "Sistotrema oblongisporum M.P.Christ. & Hauerslev, 1960"
    #>       currentCanonicalSimple     genusEpithet     specificEpithet
    #> ASV8  "Stereum ostrea"           "Stereum"        "ostrea"       
    #> ASV18 "Stereum ostrea"           "Stereum"        "ostrea"       
    #> ASV26 "Stereum hirsutum"         "Stereum"        "hirsutum"     
    #> ASV29 "Basidiodendron eyrei"     "Basidiodendron" "eyrei"        
    #> ASV32 "Sistotrema oblongisporum" "Sistotrema"     "oblongisporum"
    #>       namePublishedInYear authorship                bracketauthorship
    #> ASV8  "1838"              "Fr."                     "Blume & T.Nees" 
    #> ASV18 "1838"              "Fr."                     "Blume & T.Nees" 
    #> ASV26 "1800"              "Pers."                   "Willd."         
    #> ASV29 "1963"              "Luck-Allen"              "Wakef."         
    #> ASV32 "1960"              "M.P.Christ. & Hauerslev" NA               
    #>       scientificNameAuthorship  count_in_radius closest_distance_km
    #> ASV8  "(Blume & T.Nees) Fr."    "  1"           "40.88"            
    #> ASV18 "(Blume & T.Nees) Fr."    "  1"           "40.88"            
    #> ASV26 "(Willd.) Pers."          "442"           " 5.83"            
    #> ASV29 "(Wakef.) Luck-Allen"     "  3"           "51.95"            
    #> ASV32 "M.P.Christ. & Hauerslev" "  1"           "49.56"            
    #>       mean_distance_km total_count_in_world search_radius closest_point_lat
    #> ASV8  "40.88"          "  7331"             "100"         "48.43261"       
    #> ASV18 "40.88"          "  7331"             "100"         "48.43261"       
    #> ASV26 "56.10"          "114395"             "100"         "48.76022"       
    #> ASV29 "55.49"          "   853"             "100"         "48.40650"       
    #> ASV32 "49.56"          "  2807"             "100"         "48.42400"       
    #>       closest_point_lon sample_point_lat sample_point_lon Global_occurences
    #> ASV8  "2.278890"        "48.8"           "2.3"            " 10908"         
    #> ASV18 "2.278890"        "48.8"           "2.3"            " 10908"         
    #> ASV26 "2.248180"        "48.8"           "2.3"            "121604"         
    #> ASV29 "2.680920"        "48.8"           "2.3"            "  1156"         
    #> ASV32 "2.662000"        "48.8"           "2.3"            "  3032"

This approach provides a robust framework for validating taxa presence
and improving the reliability of your microbiome or environmental DNA
analyses.

## Session information

``` r
sessionInfo()
```

    #> R version 4.5.2 (2025-10-31)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Pop!_OS 24.04 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0  LAPACK version 3.12.0
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
    #> [1] taxinfo_0.1.2      MiscMetabar_0.15.1 divent_0.5-3       purrr_1.2.1       
    #> [5] dplyr_1.2.0        dada2_1.38.0       Rcpp_1.1.1         ggplot2_4.0.2     
    #> [9] phyloseq_1.54.2   
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] RColorBrewer_1.1-3          wk_0.9.5                   
    #>   [3] jsonlite_2.0.0              magrittr_2.0.4             
    #>   [5] farver_2.1.2                CoordinateCleaner_3.0.1    
    #>   [7] rmarkdown_2.30              fs_1.6.7                   
    #>   [9] ragg_1.5.2                  vctrs_0.7.2                
    #>  [11] multtest_2.66.0             Rsamtools_2.26.0           
    #>  [13] RCurl_1.98-1.17             terra_1.9-1                
    #>  [15] forcats_1.0.1               htmltools_0.5.9            
    #>  [17] S4Arrays_1.10.1             curl_7.0.0                 
    #>  [19] s2_1.1.9                    Rhdf5lib_1.32.0            
    #>  [21] SparseArray_1.10.9          rhdf5_2.54.1               
    #>  [23] sass_0.4.10                 KernSmooth_2.23-26         
    #>  [25] bslib_0.10.0                htmlwidgets_1.6.4          
    #>  [27] desc_1.4.3                  plyr_1.8.9                 
    #>  [29] zoo_1.8-15                  cachem_1.1.0               
    #>  [31] GenomicAlignments_1.46.0    whisker_0.4.1              
    #>  [33] igraph_2.2.2                lifecycle_1.0.5            
    #>  [35] iterators_1.0.14            pkgconfig_2.0.3            
    #>  [37] Matrix_1.7-4                R6_2.6.1                   
    #>  [39] fastmap_1.2.0               rbibutils_2.4.1            
    #>  [41] MatrixGenerics_1.22.0       digest_0.6.39              
    #>  [43] ShortRead_1.68.0            S4Vectors_0.48.0           
    #>  [45] textshaping_1.0.5           GenomicRanges_1.62.1       
    #>  [47] hwriter_1.3.2.1             vegan_2.7-3                
    #>  [49] labeling_0.4.3              urltools_1.7.3.1           
    #>  [51] httr_1.4.8                  abind_1.4-8                
    #>  [53] mgcv_1.9-4                  compiler_4.5.2             
    #>  [55] proxy_0.4-29                withr_3.0.2                
    #>  [57] S7_0.2.1                    BiocParallel_1.44.0        
    #>  [59] DBI_1.3.0                   MASS_7.3-65                
    #>  [61] DelayedArray_0.36.0         classInt_0.4-11            
    #>  [63] biomformat_1.38.3           permute_0.9-10             
    #>  [65] units_1.0-1                 oai_0.4.0                  
    #>  [67] tools_4.5.2                 otel_0.2.0                 
    #>  [69] ape_5.8-1                   rgbif_3.8.4                
    #>  [71] glue_1.8.0                  nlme_3.1-168               
    #>  [73] rhdf5filters_1.22.0         grid_4.5.2                 
    #>  [75] sf_1.1-0                    cluster_2.1.8.2            
    #>  [77] reshape2_1.4.5              ade4_1.7-23                
    #>  [79] generics_0.1.4              gtable_0.3.6               
    #>  [81] class_7.3-23                tidyr_1.3.2                
    #>  [83] data.table_1.18.2.1         utf8_1.2.6                 
    #>  [85] xml2_1.5.2                  XVector_0.50.0             
    #>  [87] BiocGenerics_0.56.0         foreach_1.5.2              
    #>  [89] pillar_1.11.1               stringr_1.6.0              
    #>  [91] rglobi_0.3.4                splines_4.5.2              
    #>  [93] lattice_0.22-9              survival_3.8-6             
    #>  [95] deldir_2.0-4                tidyselect_1.2.1           
    #>  [97] pbapply_1.7-4               Biostrings_2.78.0          
    #>  [99] knitr_1.51                  IRanges_2.44.0             
    #> [101] Seqinfo_1.0.0               SummarizedExperiment_1.40.0
    #> [103] crul_1.6.0                  stats4_4.5.2               
    #> [105] xfun_0.56                   taxize_0.10.1              
    #> [107] Biobase_2.70.0              matrixStats_1.5.0          
    #> [109] stringi_1.8.7               lazyeval_0.2.2             
    #> [111] yaml_2.3.12                 evaluate_1.0.5             
    #> [113] codetools_0.2-20            cigarillo_1.0.0            
    #> [115] httpcode_0.3.0              interp_1.1-6               
    #> [117] tibble_3.3.1                cli_3.6.5                  
    #> [119] RcppParallel_5.1.11-2       systemfonts_1.3.2          
    #> [121] Rdpack_2.6.6                jquerylib_0.1.4            
    #> [123] rnaturalearth_1.2.0         triebeard_0.4.1            
    #> [125] png_0.1-9                   parallel_4.5.2             
    #> [127] pkgdown_2.2.0               latticeExtra_0.6-31        
    #> [129] jpeg_0.1-11                 bitops_1.0-9               
    #> [131] pwalign_1.6.0               scales_1.4.0               
    #> [133] e1071_1.7-17                crayon_1.5.3               
    #> [135] geosphere_1.6-8             rlang_1.1.7
