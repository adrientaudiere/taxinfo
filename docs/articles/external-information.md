# Adding External Information: Wikipedia, GLOBI, and Custom Databases

🛈

×

**Messages**

``` popup-pre
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
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ lubridate 1.9.5     ✔ tibble    3.3.1
#> ✔ purrr     1.2.2     ✔ tidyr     1.3.2
#> ✔ readr     2.2.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(taxinfo)
```

``` r
library(MiscMetabar)
library(ggplot2)
library(tidyverse)
```

## Overview

Beyond occurrence data, `taxinfo` integrates multiple knowledge sources
to enrich your taxonomic data with Species salience in public knowledge
(**Wikipedia**), species interactions (**GLOBI**), scientific literature
(**OpenAlex**), and custom database content. This vignette demonstrates
how to leverage these functions to create comprehensive taxonomic
`@tax_table`.

## Core External Information Functions

- [`tax_get_wk_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_get_wk_info_pq.md):
  Wikipedia data and page statistics
- [`tax_globi_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_globi_pq.md):
  Species interaction data from GLOBI
- [`tax_oa_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_oa_pq.md):
  Scientific literature from OpenAlex  
- [`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md):
  Custom database integration (TAXREF, traits, etc.)
- [`tax_photos_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_photos_pq.md):
  Taxonomic images and media

**Note:** All these functions can work with either phyloseq objects or
vectors of taxonomic names (`taxnames` parameter). When using a phyloseq
object, results are automatically added to the tax_table. When using
`taxnames`, a tibble is returned.

## Wikipedia Integration

Wikipedia is a rich source of public interest and knowledge about
species. By integrating Wikipedia data, you can have a proxy of the
societal attention given to different taxa. The number of page views,
the mean page length and the number of languages in which a species is
covered can provide insights into its cultural significance measuring a
sort of species salience in public knowledge.

## Verify and Clean Taxonomic Names

🛈

×

**Messages**

``` popup-pre
#> ℹ Some GNA `data_sources` are older than 365 days; name resolution may miss
#>   recent taxa:
#>   TAXREF (id 210): last updated 2025-04-02
#> ℹ Compare update dates at <https://verifier.globalnames.org/data_sources>.
#> ✔ GNA verification summary:
#> 
#> • Total taxa in phyloseq: 20
#> 
#> • Taxa submitted for verification: 19
#> 
#> • Genus-level only taxa: 2
#> 
#> • Total matches found: 15
#> 
#> • Synonyms: 4 (including 0 uninomial)
#> 
#> • Accepted names: 11 (including 5 uninomial)
#> 
#> ℹ 5 uninomial accepted name(s) have `currentCanonicalSimple` set to "NA"
#>   (`species_only` = TRUE)
```

``` r
# Clean names first
# Keep only first 20 taxa for speed
data_clean <- prune_taxa(taxa = taxa_names(data_fungi_mini)[1:20], data_fungi_mini) |>
  gna_verifier_pq(data_sources = 210)
```

## Wikipedia Integration

The idea behind the wikipedia integration is a very brut/raw approach of
the notion of cultural keystone species (see Mattalia et al. 2025,
<https://doi.org/10.1002/pan3.10653> for a review of the concept). The
general idea is that species with more page view, page length and
language are more important in the human culture. Note that the original
notion of the cultural keystone species (CKS) is based on an
“indissoluble combination of a non-human species and one or more
sociocultural group”. Wikipedia is a very approximative and biaised way
to measure this combination, in particular due to the lack of
information on sociocultural groups.

### Basic Wikipedia Data

🛈⚠

×

**Messages**

``` popup-pre
#> ℹ Getting taxonomic IDs from Wikidata...
#> ℹ Getting page views from Wikipedia for Stereum ostrea
#> ■■■■■■■                           20% | ETA: 25s
#> ℹ Getting page views from Wikipedia for Ossicaulis lachnopus
#> ■■■■■■■                           20% | ETA: 25s
■■■■■■■■■■                        30% | ETA: 21s
#> ℹ Getting page views from Wikipedia for Stereum hirsutum
#> ■■■■■■■■■■                        30% | ETA: 21s
■■■■■■■■■■■■■                     40% | ETA: 38s
#> ℹ Getting page views from Wikipedia for Basidiodendron eyrei
#> ■■■■■■■■■■■■■                     40% | ETA: 38s
■■■■■■■■■■■■■■■■                  50% | ETA: 28s
#> ℹ Getting page views from Wikipedia for Sistotrema oblongisporum
#> ■■■■■■■■■■■■■■■■                  50% | ETA: 28s
■■■■■■■■■■■■■■■■■■■               60% | ETA: 20s
#> ℹ Getting page views from Wikipedia for Fomes fomentarius
#> ■■■■■■■■■■■■■■■■■■■               60% | ETA: 20s
■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 27s
#> ℹ Getting page views from Wikipedia for Mycena renatii
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 27s
ℹ Getting page views from Wikipedia for Cerocorticium molare
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 27s
ℹ Getting page views from Wikipedia for Aporpium canescens
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 27s
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ Getting page views from Wikipedia for Hypochnicium analogum
```

×

**Warnings**

``` popup-pre
#> Warning: `theme_idest()` was deprecated in taxinfo 0.2.0.
#> ℹ Please use `ggplotpq::theme_idest()` instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning in scale_x_log10(): log-10 transformation introduced infinite values.
```

``` r
# Add Wikipedia information (add_to_phyloseq defaults to TRUE for phyloseq objects)
data_clean_wk <- tax_get_wk_info_pq(data_clean, n_days = 30)
```

``` r
# View Wikipedia columns
data_clean_wk@tax_table |>
  as.data.frame() |>
  distinct(currentCanonicalSimple, lang, page_views, Order, page_length) |>
  filter(!is.na(currentCanonicalSimple), !is.na(page_views)) |>
  mutate(across(c(page_views, page_length, lang), as.numeric)) |>
  filter(page_views > 0) |>
  mutate(currentCanonicalSimple = factor(currentCanonicalSimple)) |>
  ggplot(aes(
    y = forcats::fct_reorder(currentCanonicalSimple, page_views),
    x = page_views, size = page_length, col = Order
  )) +
  geom_segment(aes(xend = 0, yend = currentCanonicalSimple), linewidth = 1) +
  geom_point() +
  geom_text(aes(label = lang), size = 3, color = "black") +
  scale_x_log10() +
  labs(
    title = "Species salience in public knowledge",
    x = "Number of page views during a month (log10 scale)",
    y = "Taxa (color=Order)",
    size = "Mean page length"
  ) +
  theme_idest()
```

![Lollipop chart showing Wikipedia page views for different taxa on a
log scale. Taxa are ordered by page views on the y-axis. Points are
sized by page length and colored by Order. Text labels show the number
of languages in which each species has a Wikipedia
page.](figures/unnamed-chunk-3-1.png)

plot of chunk unnamed-chunk-3

### Multilingual Wikipedia Analysis

Analyze Wikipedia coverage across languages:

🛈

×

**Messages**

``` popup-pre
#> ℹ Getting taxonomic IDs from Wikidata...
#> ℹ Getting page views from Wikipedia for Stereum ostrea
#> ■■■■■■■                           20% | ETA:  7s
#> ℹ Getting page views from Wikipedia for Ossicaulis lachnopus
#> ■■■■■■■                           20% | ETA:  7s
ℹ Getting page views from Wikipedia for Stereum hirsutum
#> ■■■■■■■                           20% | ETA:  7s
■■■■■■■■■■■■■                     40% | ETA:  6s
#> ℹ Getting page views from Wikipedia for Basidiodendron eyrei
#> ■■■■■■■■■■■■■                     40% | ETA:  6s
ℹ Getting page views from Wikipedia for Sistotrema oblongisporum
#> ■■■■■■■■■■■■■                     40% | ETA:  6s
ℹ Getting page views from Wikipedia for Fomes fomentarius
#> ■■■■■■■■■■■■■                     40% | ETA:  6s
■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s
#> ℹ Getting page views from Wikipedia for Mycena renatii
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s
ℹ Getting page views from Wikipedia for Cerocorticium molare
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s
ℹ Getting page views from Wikipedia for Aporpium canescens
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  3s
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ Getting page views from Wikipedia for Hypochnicium analogum
```

``` r
data_clean_wk2 <- tax_get_wk_info_pq(data_clean,
  languages_pages = c("en", "fr", "de", "es")
)
```

``` r

# Visualize language coverage
wiki_analysis <- data_clean_wk2@tax_table |>
  as.data.frame() |>
  select(currentCanonicalSimple, lang, page_length, page_views) |>
  mutate(across(c(lang, page_length, page_views), as.numeric)) |>
  filter(!is.na(lang)) |>
  distinct()

ggplot(wiki_analysis, aes(
  x = lang, y = log10(page_views + 1),
  size = page_length
)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(
    title = "Species salience in public knowledge",
    subtitle = "Page views in 4 different countries : en, fr, de and es",
    x = "Number of Wikipedia languages",
    y = "Page views (log10 scale)",
    size = "Mean page length"
  ) +
  theme_idest() +
  ggrepel::geom_text_repel(aes(label = currentCanonicalSimple),
    size = 3,
    max.overlaps = 10,
  )
```

![Scatter plot showing Wikipedia page views (log scale) versus number of
Wikipedia language versions for taxa across four languages (en, fr, de,
es). Points are sized by mean page length and labeled with taxonomic
names.](figures/unnamed-chunk-4-1.png)

plot of chunk unnamed-chunk-4

## Species Interactions with GLOBI

### Basic Interaction Data

GLOBI (Global Biotic Interactions) provides data on species interactions
including predation, parasitism, pollination, and more (see
[`rglobi::get_interaction_types()`](https://docs.ropensci.org/rglobi/reference/get_interaction_types.html)
for all available interaction types). Here we will add interaction data
focusing on parasitic and pathogenic relationships.

``` r
# Get interaction data from GLOBI
data_clean_globi <- tax_globi_pq(data_clean,
  interaction_types = c("parasiteOf", "pathogenOf"),
  max_interactions = 100
)

# View interaction columns
head(data_clean_globi@tax_table[, c(
  "nb",
  "target_taxon_name",
  "pathogenOf",
  "parasiteOf"
)])
```

    #> Taxonomy Table:     [6 taxa by 4 taxonomic ranks]:
    #>       nb             
    #> ASV7  NA             
    #> ASV8  NA             
    #> ASV12 NA             
    #> ASV18 NA             
    #> ASV25 NA             
    #> ASV26 "2; 2; 2; 2; 2"
    #>       target_taxon_name                                                                    
    #> ASV7  NA                                                                                   
    #> ASV8  NA                                                                                   
    #> ASV12 NA                                                                                   
    #> ASV18 NA                                                                                   
    #> ASV25 NA                                                                                   
    #> ASV26 "Broadleaved trees and shrubs; Fagus sylvatica; Quercus; Embryophyta; Prunus persica"
    #>       pathogenOf         parasiteOf                  
    #> ASV7  NA                 NA                          
    #> ASV8  NA                 NA                          
    #> ASV12 NA                 NA                          
    #> ASV18 NA                 NA                          
    #> ASV25 NA                 NA                          
    #> ASV26 "; Prunus persica" "; Fagus sylvatica; Quercus"

``` r
psmelt(data_clean_globi) |>
  group_by(taxa_name) |>
  summarise(
    Abundance = sum(Abundance),
    nb_inter = mean(map_dbl(nb, ~ sum(as.numeric(unlist(strsplit(.x, "; "))), na.rm = TRUE)), na.rm = TRUE),
    n_host_pathog = mean(map_dbl(pathogenOf, ~ stringr::str_count(.x, ";")), na.rm = TRUE),
    n_host_parasit = mean(map_dbl(parasiteOf, ~ stringr::str_count(.x, ";")), na.rm = TRUE),
    Guild = Guild[1]
  ) |>
  mutate(n_host_parasit = ifelse(is.na(n_host_parasit), 0, n_host_parasit)) |>
  mutate(n_host_pathog = ifelse(is.na(n_host_pathog), 0, n_host_pathog)) |>
  filter(nb_inter > 0) |>
  ggplot(aes(
    y = forcats::fct_reorder(taxa_name, nb_inter),
    x = nb_inter,
    color = Guild,
    size = log10(1 + Abundance)
  )) +
  geom_point() +
  geom_text(aes(label = paste(n_host_pathog, "-", n_host_parasit)),
    size = 2.5, color = "black", nudge_y = 0.2
  ) +
  theme_idest() +
  labs(
    title = "Interactions in GLOBI",
    subtitle = "First and second number indicate the number of verified taxonomic\nentity whose the taxa are respectively pathogen or parasite.",
    x = "Number of interactions",
    y = "Taxa",
    color = "Guild following FunGuild",
    size = "Number of sequences (log10)"
  )
```

![Scatter plot showing the number of interactions in GLOBI for different
fungal taxa. Points are colored by ecological guild and sized by
molecular abundance (log scale). Text labels show the count of pathogen
and parasite host relationships.](figures/unnamed-chunk-6-1.png)

plot of chunk unnamed-chunk-6

### Detailed Interaction Analysis

Get detailed interaction data for further analysis:

``` r
# Get detailed interaction tibble (not added to phyloseq)
detailed_interactions <- tax_globi_pq(data_clean,
  interaction_types = c("parasiteOf", "hasHost", "pathogenOf"),
  max_interactions = 100,
  add_to_phyloseq = FALSE
)

# Ensure all requested interaction types have a column (API may not return all)
for (.icol in c("hasHost", "parasiteOf")) {
  if (!.icol %in% names(detailed_interactions)) {
    detailed_interactions[[.icol]] <- NA_character_
  }
}

# Analyze interaction patterns
interaction_summary <- detailed_interactions |>
  separate_rows(target_taxon_name, nb, sep = ";\\s*", convert = TRUE) |>
  mutate(across(
    all_of(c("hasHost", "parasiteOf")),
    ~ stringr::str_detect(.x, stringr::fixed(target_taxon_name))
  )) |>
  mutate(interaction_type = case_when(
    parasiteOf ~ "parasiteOf",
    hasHost ~ "hasHost",
    .default = "other"
  ))

# Visualize interaction networks
ggplot(interaction_summary, aes(
  x = interaction_type, y = nb,
  fill = interaction_type
)) +
  geom_violin() +
  geom_jitter(col = "grey40", alpha = 0.2, height = 0) +
  labs(
    title = "Species Interaction Diversity",
    x = "Interaction Type",
    y = "Number of Target Taxa"
  ) +
  theme_idest() +
  theme(legend.position = "none")
```

![Violin plot with jittered points showing the distribution of number of
target taxa by interaction type (hasHost, parasiteOf, other). Each
violin is filled by interaction type.](figures/unnamed-chunk-7-1.png)

plot of chunk unnamed-chunk-7

``` r
ggplot(
  interaction_summary,
  aes(x = taxa_name, y = target_taxon_name)
) +
  geom_segment(
    aes(
      x = taxa_name, xend = target_taxon_name,
      y = 0, yend = 1,
      color = interaction_type,
      linewidth = nb
    ),
    alpha = 0.6
  ) +
  geom_point(aes(x = taxa_name, y = 0), size = 3, color = "darkred") +
  geom_point(aes(x = target_taxon_name, y = 1), size = 3, color = "darkgreen") +
  scale_linewidth_continuous(range = c(0.5, 3), name = "Number of interactions") +
  scale_color_viridis_d(name = "Interaction type") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  coord_flip() +
  labs(title = "Fungal-Plant Interaction Network")
```

![Network diagram showing fungal-plant interactions as segments
connecting fungal taxa (darkred points at bottom) to plant target taxa
(darkgreen points at top). Segments are colored by interaction type and
sized by number of interactions.](figures/unnamed-chunk-8-1.png)

plot of chunk unnamed-chunk-8

``` r
library(ggraph)

edges <- interaction_summary |>
  select(
    from = taxa_name, to = target_taxon_name,
    weight = nb, type = interaction_type
  )

nodes <- data.frame(
  name = unique(c(edges$from, edges$to))
) |>
  mutate(
    node_type = ifelse(name %in% unique(edges$from), "Fungal", "Plant")
  )

graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = weight, color = type), alpha = 0.6) +
  geom_node_point(aes(fill = node_type), size = 5, shape = 21, color = "black") +
  geom_node_text(aes(label = ifelse(node_type == "Fungal", name, "")), repel = TRUE, size = 3) +
  scale_edge_width_continuous(range = c(0.5, 3), name = "Number of interactions") +
  scale_edge_color_manual(values = c("grey60", "purple", "cyan", "green")) +
  scale_fill_manual(values = c("Plant" = "lightgreen", "Fungal" = "orange")) +
  theme_idest(grid = FALSE, axis_text_size = 0) +
  labs(
    title = "Fungal-Plant Interaction Network",
    y = "",
    x = ""
  )
```

![Network graph using Fruchterman-Reingold layout showing fungal-plant
interaction network. Fungal nodes are shown in orange and plant nodes in
light green. Edges are colored by interaction type and sized by number
of interactions. Only fungal taxa names are
labeled.](figures/unnamed-chunk-9-1.png)

plot of chunk unnamed-chunk-9

## Scientific Literature with OpenAlex

### Literature Metrics

Get publication data for your taxa and add it to the previous
wikipedia-enhanced dataset:

🛈⚠

×

**Messages**

``` popup-pre
#> ℹ Fetching OpenAlex works for taxon: Stereum ostrea
#> Fetching OpenAlex ■■■■                              10% |  ETA: 25s
#> ℹ Fetching OpenAlex works for taxon: Ossicaulis lachnopus
#> Fetching OpenAlex ■■■■                              10% |  ETA: 25s
ℹ Fetching OpenAlex works for taxon: Stereum hirsutum
#> Fetching OpenAlex ■■■■                              10% |  ETA: 25s
Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA: 31s
#> ℹ Fetching OpenAlex works for taxon: Basidiodendron eyrei
#> Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA: 31s
Fetching OpenAlex ■■■■■■■■■■■■■                     40% |  ETA: 22s
#> ℹ Fetching OpenAlex works for taxon: Sistotrema oblongisporum
#> Fetching OpenAlex ■■■■■■■■■■■■■                     40% |  ETA: 22s
ℹ Fetching OpenAlex works for taxon: Fomes fomentarius
#> Fetching OpenAlex ■■■■■■■■■■■■■                     40% |  ETA: 22s
Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■               60% |  ETA: 20s
#> ℹ Fetching OpenAlex works for taxon: Mycena renatii
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■               60% |  ETA: 20s
#> ℹ Fetching OpenAlex works for taxon: Cerocorticium molare
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■               60% |  ETA: 20s
#> ℹ Fetching OpenAlex works for taxon: Aporpium canescens
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■               60% |  ETA: 20s
ℹ Fetching OpenAlex works for taxon: Hypochnicium analogum
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■               60% |  ETA: 20s
```

×

**Warnings**

``` popup-pre
#> Warning: No records found!
#> Warning: No records found!
```

``` r
data_clean_oa <- tax_oa_pq(data_clean_wk)
```

``` r

head(data_clean_oa@tax_table[, c(
  "n_doi",
  "list_doi",
  "taxa_name"
)])
```

    #> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
    #>       n_doi
    #> ASV7  NA   
    #> ASV8  " 39"
    #> ASV12 NA   
    #> ASV18 " 39"
    #> ASV25 "  5"
    #> ASV26 "262"
    #>       list_doi                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
    #> ASV7  NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    #> ASV8  "https://doi.org/10.1155/2014/815495; https://doi.org/10.1016/j.phytochem.2012.04.009; https://doi.org/10.1021/ol2019778; https://doi.org/10.1038/ja.2006.61; https://doi.org/10.4489/myco.2007.35.4.210; https://doi.org/10.1021/acs.jnatprod.6b00647; https://doi.org/10.4014/jmb.1112.12011; https://doi.org/10.4061/2011/749518; https://doi.org/10.1007/s13205-019-1955-6; https://doi.org/10.1201/b19978-17; https://doi.org/10.1007/s13205-015-0301-x; https://doi.org/10.1186/s11671-026-04461-5; https://doi.org/10.4314/ajb.v7i8.58632; https://doi.org/10.1080/00275514.1971.12019168; https://doi.org/10.2307/3758046; https://doi.org/10.1016/s0254-6299(15)30824-3; https://doi.org/10.1007/s10267-004-0215-7; https://doi.org/10.4489/myco.2008.36.2.114; https://doi.org/10.1002/chin.200703164; https://doi.org/10.1080/10889868.2022.2029823; https://doi.org/10.1002/slct.202505744; https://doi.org/10.3852/09-008; https://doi.org/10.5941/myco.2012.40.2.134; https://doi.org/10.13057/biodiv/d180213; https://doi.org/10.23880/ipcm-16000169; https://doi.org/10.1007/978-4-431-67008-7_12; https://doi.org/10.35580/bionature.v12i2.1402; https://doi.org/10.1016/j.funeco.2023.101314; https://doi.org/10.1615/intjmedmushr.v7.i3.230; https://doi.org/10.47371/mycosci.myc46097; https://doi.org/10.36706/fpbio.v3i1.4966; https://doi.org/10.7747/jfes.2016.32.2.158; https://doi.org/10.30550/j.lil/1807; https://doi.org/10.1615/intjmedmushrooms.v7.i3.230; https://doi.org/10.4489/kjm.2014.42.4.322; https://doi.org/10.1038/ja.2006.61; https://doi.org/10.1111/j.1748-5967.2011.00438.x; https://doi.org/10.22271/27889289.2024.v4.i1c.131; https://doi.org/10.21472/bjbs.v11n25-008"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
    #> ASV12 NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    #> ASV18 "https://doi.org/10.1155/2014/815495; https://doi.org/10.1016/j.phytochem.2012.04.009; https://doi.org/10.1021/ol2019778; https://doi.org/10.1038/ja.2006.61; https://doi.org/10.4489/myco.2007.35.4.210; https://doi.org/10.1021/acs.jnatprod.6b00647; https://doi.org/10.4014/jmb.1112.12011; https://doi.org/10.4061/2011/749518; https://doi.org/10.1007/s13205-019-1955-6; https://doi.org/10.1201/b19978-17; https://doi.org/10.1007/s13205-015-0301-x; https://doi.org/10.1186/s11671-026-04461-5; https://doi.org/10.4314/ajb.v7i8.58632; https://doi.org/10.1080/00275514.1971.12019168; https://doi.org/10.2307/3758046; https://doi.org/10.1016/s0254-6299(15)30824-3; https://doi.org/10.1007/s10267-004-0215-7; https://doi.org/10.4489/myco.2008.36.2.114; https://doi.org/10.1002/chin.200703164; https://doi.org/10.1080/10889868.2022.2029823; https://doi.org/10.1002/slct.202505744; https://doi.org/10.3852/09-008; https://doi.org/10.5941/myco.2012.40.2.134; https://doi.org/10.13057/biodiv/d180213; https://doi.org/10.23880/ipcm-16000169; https://doi.org/10.1007/978-4-431-67008-7_12; https://doi.org/10.35580/bionature.v12i2.1402; https://doi.org/10.1016/j.funeco.2023.101314; https://doi.org/10.1615/intjmedmushr.v7.i3.230; https://doi.org/10.47371/mycosci.myc46097; https://doi.org/10.36706/fpbio.v3i1.4966; https://doi.org/10.7747/jfes.2016.32.2.158; https://doi.org/10.30550/j.lil/1807; https://doi.org/10.1615/intjmedmushrooms.v7.i3.230; https://doi.org/10.4489/kjm.2014.42.4.322; https://doi.org/10.1038/ja.2006.61; https://doi.org/10.1111/j.1748-5967.2011.00438.x; https://doi.org/10.22271/27889289.2024.v4.i1c.131; https://doi.org/10.21472/bjbs.v11n25-008"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
    #> ASV25 "https://doi.org/10.1007/s11557-012-0866-2; https://doi.org/10.1016/j.myc.2017.07.008; https://doi.org/10.30796/angv.2018.3; https://doi.org/10.1134/s2079086425600961; https://doi.org/10.7868/s3034542125010058"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    #> ASV26 "https://doi.org/10.1155/2015/789089; https://doi.org/10.1016/j.enconman.2017.03.021; https://doi.org/10.1016/s0031-9422(99)00565-8; https://doi.org/10.1016/s0007-1536(81)80007-1; https://doi.org/10.1007/s11306-007-0100-4; https://doi.org/10.1016/j.bmcl.2007.08.072; https://doi.org/10.1016/s0007-1536(85)80118-2; https://doi.org/10.1248/bpb.28.201; https://doi.org/10.1016/j.foodchem.2013.07.124; https://doi.org/10.7164/antibiotics.55.208; https://doi.org/10.1038/ja.2006.16; https://doi.org/10.1016/j.phytol.2019.02.007; https://doi.org/10.1016/s0007-1536(75)80146-x; https://doi.org/10.1055/s-0034-1382828; https://doi.org/10.1021/acs.orglett.5b01356; https://doi.org/10.1016/s0007-1536(75)80145-8; https://doi.org/10.7164/antibiotics.54.521; https://doi.org/10.1371/journal.pone.0255899; https://doi.org/10.1016/j.fct.2017.05.036; https://doi.org/10.1016/s0953-7562(09)80110-x; https://doi.org/10.1016/j.fitote.2018.05.026; https://doi.org/10.1021/ol502441n; https://doi.org/10.1128/aem.00036-18; https://doi.org/10.1016/j.biortech.2012.03.047; https://doi.org/10.1016/s0007-1536(85)80070-x; https://doi.org/10.1007/s00253-010-2668-2; https://doi.org/10.1111/j.1469-8137.1985.tb03674.x; https://doi.org/10.5941/myco.2015.43.3.297; https://doi.org/10.1080/03639040500530026; https://doi.org/10.1099/00221287-89-2-229; https://doi.org/10.1016/s0007-1536(77)80183-6; https://doi.org/10.1002/cbdv.202100409; https://doi.org/10.1098/rstb.1897.0013; https://doi.org/10.1016/j.phytochem.2021.112852; https://doi.org/10.4014/jmb.1210.10060; https://doi.org/10.22092/ari.2019.126283.1340; https://doi.org/10.1099/00221287-131-1-207; https://doi.org/10.1016/0038-0717(78)90031-7; https://doi.org/10.1080/10286020.2014.959439; https://doi.org/10.1111/j.1469-8137.1990.tb00929.x; https://doi.org/10.1016/j.bioorg.2020.103760; https://doi.org/10.1016/j.ijbiomac.2020.01.097; https://doi.org/10.1080/03601230600616072; https://doi.org/10.1016/j.phytochem.2022.113227; https://doi.org/10.1111/j.1469-8137.1989.tb00362.x; https://doi.org/10.1080/10412905.2010.9700277; https://doi.org/10.1080/14786419.2020.1779266; https://doi.org/10.1016/j.lwt.2022.113179; https://doi.org/10.3390/foods11111587; https://doi.org/10.1080/14786419.2022.2047046; https://doi.org/10.1099/00221287-89-2-235; https://doi.org/10.3390/foods12132507; https://doi.org/10.4067/s0717-97072016000400015; https://doi.org/10.1016/j.phytochem.2024.114253; https://doi.org/10.1080/02772249909358816; https://doi.org/10.1186/s40643-025-00842-3; https://doi.org/10.2478/johr-2022-0003; https://doi.org/10.1007/s13659-025-00505-y; https://doi.org/10.1098/rspl.1897.0109; https://doi.org/10.1016/s0007-1536(85)80015-2; https://doi.org/10.5109/20309; https://doi.org/10.1016/s1130-1406(07)70059-1; https://doi.org/10.1016/j.jafr.2025.102101; https://doi.org/10.1016/j.eti.2019.100369; https://doi.org/10.1128/spectrum.02624-22; https://doi.org/10.1021/np010602b; https://doi.org/10.1002/(sici)1522-2675(19990908)82:9<1418::aid-hlca1418>3.0.co;2-o; https://doi.org/10.1023/a:1008638409410; https://doi.org/10.2298/gsf0591179m; https://doi.org/10.1007/bf02814716; https://doi.org/10.1080/14786419.2019.1687478; https://doi.org/10.1080/00021369.1976.10862288; https://doi.org/10.1111/j.1574-6968.2002.tb11245.x; https://doi.org/10.1007/bf00167925; https://doi.org/10.1016/s0040-4020(01)92573-6; https://doi.org/10.1016/j.cropro.2016.07.014; https://doi.org/10.1016/s0007-1536(82)80155-1; https://doi.org/10.1007/s00248-004-0240-2; https://doi.org/10.1007/bf01086322; https://doi.org/10.1007/bf02906805; https://doi.org/10.1080/00275514.1971.12019168; https://doi.org/10.1007/bf02826566; https://doi.org/10.1099/00221287-138-6-1147; https://doi.org/10.14601/phytopathol_mediterr-1621; https://doi.org/10.1039/c0jm01144d; https://doi.org/10.1002/cbic.201300349; https://doi.org/10.1080/00275514.1994.12026373; https://doi.org/10.1111/j.1574-6968.1992.tb05493.x; https://doi.org/10.14601/phytopathol_mediterr-1552; https://doi.org/10.1007/s00226-006-0087-4; https://doi.org/10.1111/j.1469-8137.1985.tb02825.x; https://doi.org/10.1016/s0168-1656(00)00264-9; https://doi.org/10.1263/jbb.106.162; https://doi.org/10.1016/j.ygeno.2019.04.012; https://doi.org/10.2307/3758046; https://doi.org/10.2307/4111529; https://doi.org/10.1007/s12649-010-9052-4; https://doi.org/10.2307/3760718; https://doi.org/10.1038/211868a0; https://doi.org/10.1016/s0254-6299(15)30824-3; https://doi.org/10.1007/978-3-031-23031-8_125; https://doi.org/10.1016/j.ibiod.2008.03.010; https://doi.org/10.1016/s0045-6535(97)00363-9; https://doi.org/10.1016/j.biortech.2004.01.007; https://doi.org/10.36253/phyto-4911; https://doi.org/10.4067/s0718-16202008000200005; https://doi.org/10.14601/phytopathol_mediterr-1622; https://doi.org/10.14601/phytopathol_mediterr-1574; https://doi.org/10.36253/phyto-4846; https://doi.org/10.1400/68063; https://doi.org/10.1016/s2707-3688(23)00041-9; https://doi.org/10.1111/j.1469-8137.1985.tb02823.x; https://doi.org/10.1400/57803; https://doi.org/10.1016/j.ecoenv.2012.01.013; https://doi.org/10.37489/0235-2990-2025-70-7-8-10-18; https://doi.org/10.1002/chin.200022202; https://doi.org/10.14601/phytopathol_mediterr-16293; https://doi.org/10.32859/era.34.44.1-21; https://doi.org/10.1002/chin.200002252; https://doi.org/10.1002/chin.200809202; https://doi.org/10.1016/s0953-7562(09)81261-6; https://doi.org/10.5530/jam.2.6.6; https://doi.org/10.1080/03650340.2016.1155699; https://doi.org/10.14601/phytopathol_mediterr-1531; https://doi.org/10.1007/s40974-019-00123-8; https://doi.org/10.33585/cmy.18303; https://doi.org/10.1002/chin.200233249; https://doi.org/10.1007/s11270-014-1872-6; https://doi.org/10.14601/phytopathol_mediterr-1537; https://doi.org/10.1002/chin.200140248; https://doi.org/10.1002/chin.200634183; https://doi.org/10.1016/j.tetlet.2005.11.150; https://doi.org/10.3390/jof12010041; https://doi.org/10.15407/biotech6.03.116; https://doi.org/10.3390/ijms24032318; https://doi.org/10.3390/pathogens11091006; https://doi.org/10.1007/bf02628843; https://doi.org/10.17099/jffiu.65672; https://doi.org/10.1016/j.heliyon.2024.e28709; https://doi.org/10.1002/chin.201514254; https://doi.org/10.5424/sjar/2008064-357; https://doi.org/10.1017/s0953756200003579; https://doi.org/10.1111/icad.12055; https://doi.org/10.1111/j.1469-8137.1979.tb02677.x; https://doi.org/10.1016/j.bse.2026.105268; https://doi.org/10.1007/s00248-004-0075-x; https://doi.org/10.1007/s11676-018-0612-y; https://doi.org/10.1007/s13659-016-0096-4; https://doi.org/10.1111/j.1469-8137.1990.tb00930.x; https://doi.org/10.1080/02827589809383004; https://doi.org/10.2323/jgam.59.279; https://doi.org/10.3390/ijms20235990; https://doi.org/10.1400/57806; https://doi.org/10.1080/02772249509358218; https://doi.org/10.1016/s0269-915x(99)80044-5; https://doi.org/10.1080/09593330.2012.760654; https://doi.org/10.1016/s0960-8524(99)00040-1; https://doi.org/10.1016/s0953-7562(09)80755-7; https://doi.org/10.1515/pjen-2016-0026; https://doi.org/10.3989/ajbm.2292; https://doi.org/10.1016/j.foreco.2012.11.010; https://doi.org/10.1007/bf02617665; https://doi.org/10.1016/s0378-1097(02)00710-3; https://doi.org/10.3390/f14102029; https://doi.org/10.1007/s00253-023-12621-1; https://doi.org/10.1080/10826068.2022.2109048; https://doi.org/10.3390/plants12132553; https://doi.org/10.3390/jof10080557; https://doi.org/10.3390/antibiotics11050622; https://doi.org/10.1271/bbb1961.40.559; https://doi.org/10.1007/s10532-023-10045-2; https://doi.org/10.2202/1542-6580.1935; https://doi.org/10.1400/14576; https://doi.org/10.4155/bfs.11.129; https://doi.org/10.7764/rcia.v35i2.359; https://doi.org/10.18470/1992-1098-2020-4-75-98; https://doi.org/10.1111/efp.12499; https://doi.org/10.1016/s0007-1536(84)80078-9; https://doi.org/10.35580/bionature.v12i2.1402; https://doi.org/10.5962/p.416934; https://doi.org/10.1111/j.1365-3059.2008.01898.x; https://doi.org/10.4028/www.scientific.net/amr.778.818; https://doi.org/10.1080/10934529.2012.672317; https://doi.org/10.4067/s0718-221x2014005000012; https://doi.org/10.1016/j.mycres.2005.12.004; https://doi.org/10.1016/j.apsb.2017.03.001; https://doi.org/10.1094/pd-90-0835a; https://doi.org/10.14601/phytopathol_mediterr-1854; https://doi.org/10.5586/am.1999.022; https://doi.org/10.14601/phytopathol_mediterr-1848; https://doi.org/10.1016/0047-7206(76)90001-7; https://doi.org/10.1080/10934529.2015.1030294; https://doi.org/10.51258/rjh.2021.18; https://doi.org/10.34101/actaagrar/72/1602; https://doi.org/10.1111/efp.12634; https://doi.org/10.3161/15052249pje2020.68.1.002; https://doi.org/10.3389/fmicb.2023.1148750; https://doi.org/10.1016/j.fbio.2025.106963; https://doi.org/10.33064/iycuaa2013574011; https://doi.org/10.5658/wood.2013.41.1.19; https://doi.org/10.2298/gsf0591031m; https://doi.org/10.4102/abc.v10i4.1557; https://doi.org/10.36490/agri.v4i2.169; https://doi.org/10.33585/cmy.38303; https://doi.org/10.3897/ap.2.e57555; https://doi.org/10.1560/ijps.56.4.349; https://doi.org/10.1080/00021369.1976.10862080; https://doi.org/10.2478/ffp-2020-0009; https://doi.org/10.1007/978-981-97-7110-3_21; https://doi.org/10.15177/seefor.20-17; https://doi.org/10.1016/j.micres.2025.128374; https://doi.org/10.7747/jfes.2016.32.2.158; https://doi.org/10.2524/jtappij.46.426; https://doi.org/10.15421/40270608; https://doi.org/10.2298/zmspn1324367m; https://doi.org/10.51826/piper.v13i25.99; https://doi.org/10.1088/1755-1315/914/1/012077; https://doi.org/10.71024/ecobios/2024/v1i1/15; https://doi.org/10.35414/akufemubid.871487; https://doi.org/10.1016/j.funbio.2025.101661; https://doi.org/10.15835/buasvmcn-agr:11146; https://doi.org/10.1016/j.bmcl.2007.08.072; https://doi.org/10.22370/bolmicol.1998.13.0.962; https://doi.org/10.1038/hdy.1977.87; https://doi.org/10.14601/phytopathol_mediterr-1739; https://doi.org/10.1111/j.1748-5967.2011.00438.x; https://doi.org/10.5281/zenodo.2547732; https://doi.org/10.1080/11263506509430811; https://doi.org/10.1080/01811789.1980.10826458; https://doi.org/10.1093/oxfordjournals.aob.a087760; https://doi.org/10.14288/1.0106036; https://doi.org/10.1007/978-94-007-5634-2_167; https://doi.org/10.51419/202134415.; https://doi.org/10.65999/macrofungi.2025.1; https://doi.org/10.3390/app16063031; https://doi.org/10.24127/edubiolock.v4i3.4399; https://doi.org/10.1016/j.funbio.2025.101667; https://doi.org/10.54026/aart/1044; https://doi.org/10.7905/bbmspu.v3i3.727; https://doi.org/10.16955/bkb.07560; https://doi.org/10.3390/genes17030296; https://doi.org/10.15421/20133_60; https://doi.org/10.5937/sustfor2490119v; https://doi.org/10.13287/j.1001-9332.202001.037; https://doi.org/10.3390/foods11111587; https://doi.org/10.1098/rspl.1897.0141; https://doi.org/10.3390/agronomy15081851; https://doi.org/10.15835/buasvmcn-hort:7017; https://doi.org/10.3390/f15050850; https://doi.org/10.1016/0008-8749(82)90390-2; https://doi.org/10.1007/s11274-025-04293-y; https://doi.org/10.22055/ppr.2016.11974; https://doi.org/10.2478/eces-2025-0020; https://doi.org/10.34736/fnc.2025.129.2.001.08-17; https://doi.org/10.21266/2079-4304.2025.254.256-278; https://doi.org/10.6084/m9.figshare.791635.v2; https://doi.org/10.3389/fmicb.2026.1669303; https://doi.org/10.5731/pdajpst.2022.012769; https://doi.org/10.1016/b978-0-443-30086-8.00003-6; https://doi.org/10.32782/agrobio.2024.1.3; https://doi.org/10.17192/z2024.0479; https://doi.org/10.5281/zenodo.14180779"
    #>       taxa_name             
    #> ASV7  "NA"                  
    #> ASV8  "Stereum ostrea"      
    #> ASV12 "NA"                  
    #> ASV18 "Stereum ostrea"      
    #> ASV25 "Ossicaulis lachnopus"
    #> ASV26 "Stereum hirsutum"

### Research Interest Analysis

Analyze research patterns:

``` r
data_clean_oa@tax_table |>
  as.data.frame() |>
  select(
    currentCanonicalSimple,
    n_doi,
    n_citation,
    page_views,
    lang,
    Family
  ) |>
  mutate(across(any_of(c("n_doi", "n_citation", "page_views", "lang")), as.numeric)) |>
  distinct(currentCanonicalSimple, .keep_all = TRUE) |>
  ggplot(aes(
    x = log10(n_doi + 1),
    y = log10(page_views + 1)
  )) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point(aes(size = n_citation / n_doi, color = lang), alpha = 0.6) +
  labs(
    title = "Scientific Interest vs Public Interest",
    x = "Number of Publications (log10)",
    y = "Wikipedia Page Views (log10)",
    size = "Mean nb of citation"
  ) +
  ggrepel::geom_text_repel(aes(label = currentCanonicalSimple), size = 3, color = "black", fontface = "italic") +
  theme_idest() +
  ggpmisc::stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  )
```

## Custom Database Integration

### Fungal Traits Database

Integrate trait databases using
[`tax_info_pq()`](https://adrientaudiere.github.io/taxinfo/reference/tax_info_pq.md).
Here we will use a bigger phyloseq object to found more informations in
the mini-trait database. When using true database, you should find more
information, even with less taxa.

🛈

×

**Messages**

``` popup-pre
#> ℹ Some GNA `data_sources` are older than 365 days; name resolution may miss
#>   recent taxa:
#>   TAXREF (id 210): last updated 2025-04-02
#> ℹ Compare update dates at <https://verifier.globalnames.org/data_sources>.
#> ✔ GNA verification summary:
#> 
#> • Total taxa in phyloseq: 1420
#> 
#> • Taxa submitted for verification: 1010
#> 
#> • Genus-level only taxa: 359
#> 
#> • Total matches found: 300
#> 
#> • Synonyms: 16 (including 1 uninomial)
#> 
#> • Accepted names: 284 (including 148 uninomial)
#> 
#> ℹ 148 uninomial accepted name(s) have `currentCanonicalSimple` set to "NA"
#>   (`species_only` = TRUE)
#> ✔ Added 18 columns from '/home/adrien/R/x86_64-pc-linux-gnu-library/4.6/taxinfo/extdata/fun_trait_mini.csv' with information for 81 taxa in the tax_table slot of the phyloseq object
```

``` r
data_clean_full <- gna_verifier_pq(data_fungi, data_sources = 210)
```

``` r

# Load fungal traits database
fungal_traits <- system.file("extdata", "fun_trait_mini.csv",
  package = "taxinfo"
)

# Add trait information
data_clean_ft <- tax_info_pq(data_clean_full,
  taxonomic_rank = "genusEpithet",
  file_name = fungal_traits,
  csv_taxonomic_rank = "GENUS",
  col_prefix = "ft_",
  sep = ";"
)
```

``` r

# View trait columns
data_clean_ft@tax_table |>
  as.data.frame() |>
  pull(ft_primary_lifestyle) |>
  table()
```

    #> 
    #>                     animal_parasite        lichenized litter_saprotroph 
    #>                 4                 1                10                24 
    #>      mycoparasite    plant_pathogen   soil_saprotroph        sooty_mold 
    #>                 1                 6                 1                 1 
    #>   wood_saprotroph 
    #>                33

### Example Trait Visualization

Here’s an example of fungal livestyle distribution

    #> Error in `summarise()`:
    #> ℹ In argument: `lifestyle = unique(ft_primary_lifestyle)`.
    #> ℹ In group 152: `currentCanonicalSimple = NA`.
    #> Caused by error:
    #> ! `lifestyle` must be size 1, not 8.
    #> ℹ To return more or less than 1 row per group, use `reframe()`.

### TAXREF Integration

For French taxonomic data, integrate TAXREF:

🛈

×

**Messages**

``` popup-pre
#> ✔ Added 43 columns from '/home/adrien/R/x86_64-pc-linux-gnu-library/4.6/taxinfo/extdata/TAXREFv18_fungi.csv' with information for 13 taxa in the tax_table slot of the phyloseq object
```

``` r
# Load TAXREF data (example file)
taxref_file <- system.file("extdata", "TAXREFv18_fungi.csv",
  package = "taxinfo"
)

# Add TAXREF information
data_clean_taxref <- tax_info_pq(data_clean,
  file_name = taxref_file,
  csv_taxonomic_rank = "LB_NOM",
  csv_cols_select = NULL,
  col_prefix = "taxref_"
)
```

⚠

×

**Warnings**

``` popup-pre
#> Warning: Removed 8 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

``` r
psm <- psmelt(data_clean_taxref) |>
  group_by(currentCanonicalSimple) |>
  mutate(across(everything(), ~ replace(., . == "" | . == "NA", NA))) |>
  filter(!is.na(currentCanonicalSimple)) |>
  summarise(
    taxref_FR = unique(taxref_FR),
    taxref_HABITAT = unique(taxref_HABITAT),
    occurence = sum(Abundance > 0, na.rm = TRUE),
    Abundance = sum(Abundance, na.rm = T),
    Order = unique(taxref_ORDRE),
    taxref_NOM_VERN = unique(taxref_NOM_VERN)
  ) |>
  filter(!is.na(Order))

ggplot(psm, aes(
  x = forcats::fct_reorder(currentCanonicalSimple, occurence),
  y = 1 + occurence,
  fill = Order
)) +
  geom_col() +
  geom_text(
    aes(
      label = currentCanonicalSimple,
      fontface = ifelse(is.na(taxref_FR), "italic", "bold.italic")
    ),
    y = -1, hjust = 1, size = 2.5
  ) +
  geom_text(aes(label = taxref_NOM_VERN, y = 2 + occurence), size = 3, color = "black", hjust = 0) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Frequences of Taxa with French common names",
    subtitle = "Bold taxa are already known in France (TAXREF).",
    x = "Taxa",
    y = "Number of occurences (samples)"
  ) +
  theme_idest() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.35, 0.05)))
```

![Horizontal bar chart showing number of sample of taxa. Bars are
colored by Order. Taxa known in France (i.e. present in TAXREF) are
shown in bold. French vernacular names are shown as text
labels.](figures/unnamed-chunk-15-1.png)

plot of chunk unnamed-chunk-15

## Comprehensive Data Integration

### Multi-Source Enrichment

Combine multiple data sources for comprehensive profiles:

🛈

×

**Messages**

``` popup-pre
#> ℹ Some GNA `data_sources` are older than 365 days; name resolution may miss
#>   recent taxa:
#>   TAXREF (id 210): last updated 2025-04-02
#> ℹ Compare update dates at <https://verifier.globalnames.org/data_sources>.
#> ✔ GNA verification summary:
#> 
#> • Total taxa in phyloseq: 45
#> 
#> • Taxa submitted for verification: 37
#> 
#> • Genus-level only taxa: 2
#> 
#> • Total matches found: 25
#> 
#> • Synonyms: 4 (including 0 uninomial)
#> 
#> • Accepted names: 21 (including 6 uninomial)
#> 
#> ℹ 6 uninomial accepted name(s) have `currentCanonicalSimple` set to "NA"
#>   (`species_only` = TRUE)
#> 
#> ℹ Processing GBIF occurrences for Stereum ostrea
#> 
#> ℹ Processing GBIF occurrences for Ossicaulis lachnopus
#> 
#> ■■■■■■                            17% | ETA:  6s
#> 
#> ℹ Processing GBIF occurrences for Stereum hirsutum
#> 
#> ■■■■■■                            17% | ETA:  6s
#> ℹ Processing GBIF occurrences for Basidiodendron eyrei
#> 
#> ■■■■■■                            17% | ETA:  6s
#> ℹ Processing GBIF occurrences for Sistotrema oblongisporum
#> 
#> ■■■■■■                            17% | ETA:  6s
#> ℹ Processing GBIF occurrences for Fomes fomentarius
#> 
#> ■■■■■■                            17% | ETA:  6s
#> ■■■■■■■■■■■■■                     39% | ETA:  5s
#> 
#> ℹ Processing GBIF occurrences for Cerocorticium molare
#> 
#> ■■■■■■■■■■■■■                     39% | ETA:  5s
#> ℹ Processing GBIF occurrences for Aporpium canescens
#> 
#> ■■■■■■■■■■■■■                     39% | ETA:  5s
#> ℹ Processing GBIF occurrences for Hypochnicium analogum
#> 
#> ■■■■■■■■■■■■■                     39% | ETA:  5s
#> ℹ Processing GBIF occurrences for Hyphoderma roseocremeum
#> 
#> ■■■■■■■■■■■■■                     39% | ETA:  5s
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> 
#> ℹ Processing GBIF occurrences for Hyphoderma setigerum
#> 
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> ℹ Processing GBIF occurrences for Trametes versicolor
#> 
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> ℹ Processing GBIF occurrences for Peniophora versiformis
#> 
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> ℹ Processing GBIF occurrences for Exidia glandulosa
#> 
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> ℹ Processing GBIF occurrences for Peniophorella pubera
#> 
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> ℹ Processing GBIF occurrences for Auricularia mesenterica
#> 
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA:  4s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> 
#> ℹ Processing GBIF occurrences for Hericium coralloides
#> 
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Processing GBIF occurrences for Xylodon flaviporus
#> 
#> ℹ Getting taxonomic IDs from Wikidata...
#> 
#> ℹ Getting page views from Wikipedia for Stereum ostrea
#> 
#> ■■■■                              11% | ETA: 50s
#> 
#> ℹ Getting page views from Wikipedia for Ossicaulis lachnopus
#> 
#> ■■■■                              11% | ETA: 50s
#> ■■■■■■                            16% | ETA: 46s
#> 
#> ℹ Getting page views from Wikipedia for Stereum hirsutum
#> 
#> ■■■■■■                            16% | ETA: 46s
#> ■■■■■■■                           21% | ETA:  2m
#> 
#> ℹ Getting page views from Wikipedia for Basidiodendron eyrei
#> 
#> ■■■■■■■                           21% | ETA:  2m
#> ■■■■■■■■■                         26% | ETA:  1m
#> 
#> ℹ Getting page views from Wikipedia for Sistotrema oblongisporum
#> 
#> ■■■■■■■■■                         26% | ETA:  1m
#> ■■■■■■■■■■                        32% | ETA:  1m
#> 
#> ℹ Getting page views from Wikipedia for Fomes fomentarius
#> 
#> ■■■■■■■■■■                        32% | ETA:  1m
#> ■■■■■■■■■■■■                      37% | ETA:  2m
#> 
#> ℹ Getting page views from Wikipedia for Mycena renatii
#> 
#> ■■■■■■■■■■■■                      37% | ETA:  2m
#> ℹ Getting page views from Wikipedia for Cerocorticium molare
#> 
#> ■■■■■■■■■■■■                      37% | ETA:  2m
#> ℹ Getting page views from Wikipedia for Aporpium canescens
#> 
#> ■■■■■■■■■■■■                      37% | ETA:  2m
#> ℹ Getting page views from Wikipedia for Hypochnicium analogum
#> 
#> ■■■■■■■■■■■■                      37% | ETA:  2m
#> ℹ Getting page views from Wikipedia for Hyphoderma roseocremeum
#> 
#> ■■■■■■■■■■■■                      37% | ETA:  2m
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 40s
#> 
#> ℹ Getting page views from Wikipedia for Hyphoderma setigerum
#> 
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 40s
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 33s
#> 
#> ℹ Getting page views from Wikipedia for Trametes versicolor
#> 
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 33s
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 35s
#> 
#> ℹ Getting page views from Wikipedia for Peniophora versiformis
#> 
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 35s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 27s
#> 
#> ℹ Getting page views from Wikipedia for Exidia glandulosa
#> 
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 27s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 22s
#> 
#> ℹ Getting page views from Wikipedia for Peniophorella pubera
#> 
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 22s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA: 14s
#> 
#> ℹ Getting page views from Wikipedia for Auricularia mesenterica
#> 
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA: 14s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  7s
#> 
#> ℹ Getting page views from Wikipedia for Hericium coralloides
#> 
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  7s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Getting page views from Wikipedia for Xylodon flaviporus
#> 
#> ✔ Added 43 columns from '/home/adrien/R/x86_64-pc-linux-gnu-library/4.6/taxinfo/extdata/TAXREFv18_fungi.csv' with information for 28 taxa in the tax_table slot of the phyloseq object
```

``` r
# Complete multi-source enrichment
data_fungi_full <- data_fungi_mini |>
  gna_verifier_pq(data_sources = 210) |>
  tax_gbif_occur_pq() |>
  tax_get_wk_info_pq() |>
  tax_oa_pq(count_only = TRUE) |>
  tax_globi_pq(
    interaction_types = c("parasiteOf", "pathogenOf"),
    max_interactions = 20
  ) |>
  tax_info_pq(
    file_name = taxref_file,
    csv_taxonomic_rank = "LB_NOM",
    csv_cols_select = NULL,
    col_prefix = "taxref_"
  )
```

``` r

# View enriched tax_table
data_fungi_full@tax_table[1:3, ]
```

    #> Taxonomy Table:     [3 taxa by 76 taxonomic ranks]:
    #>       taxref_LB_NOM    taxref_REGNE taxref_PHYLUM   taxref_CLASSE taxref_ORDRE 
    #> ASV7  "NA"             NA           NA              NA            NA           
    #> ASV8  "Stereum ostrea" "Fungi"      "Basidiomycota" ""            "Polyporales"
    #> ASV12 "NA"             NA           NA              NA            NA           
    #>       taxref_FAMILLE taxref_SOUS_FAMILLE taxref_TRIBU taxref_GROUP1_INPN
    #> ASV7  NA             NA                  NA           NA                
    #> ASV8  "Stereaceae"   ""                  ""           "Basidiomycètes"  
    #> ASV12 NA             NA                  NA           NA                
    #>       taxref_GROUP2_INPN taxref_GROUP3_INPN taxref_CD_NOM taxref_CD_TAXSUP
    #> ASV7  NA                 NA                 NA            NA              
    #> ASV8  "Autres"           "Autres"           "900725"      "197988"        
    #> ASV12 NA                 NA                 NA            NA              
    #>       taxref_CD_SUP taxref_CD_REF taxref_CD_BA taxref_RANG
    #> ASV7  NA            NA            NA           NA         
    #> ASV8  "197988"      "900725"      "900727"     "ES"       
    #> ASV12 NA            NA            NA           NA         
    #>       taxref_LB_AUTEUR             taxref_NOMENCLATURAL_COMMENT
    #> ASV7  NA                           NA                          
    #> ASV8  "(Blume & T.Nees) Fr., 1838" ""                          
    #> ASV12 NA                           NA                          
    #>       taxref_NOM_COMPLET                         
    #> ASV7  NA                                         
    #> ASV8  "Stereum ostrea (Blume & T.Nees) Fr., 1838"
    #> ASV12 NA                                         
    #>       taxref_NOM_COMPLET_HTML                           
    #> ASV7  NA                                                
    #> ASV8  "<i>Stereum ostrea</i> (Blume & T.Nees) Fr., 1838"
    #> ASV12 NA                                                
    #>       taxref_NOM_VALIDE                           taxref_NOM_VERN
    #> ASV7  NA                                          NA             
    #> ASV8  "Stereum ostrea (Blume & T.Nees) Fr., 1838" ""             
    #> ASV12 NA                                          NA             
    #>       taxref_NOM_VERN_ENG taxref_HABITAT taxref_FR taxref_GF taxref_MAR
    #> ASV7  NA                  NA             NA        NA        NA        
    #> ASV8  ""                  "3"            ""        ""        "D"       
    #> ASV12 NA                  NA             NA        NA        NA        
    #>       taxref_GUA taxref_SM taxref_SB taxref_SPM taxref_MAY taxref_EPA
    #> ASV7  NA         NA        NA        NA         NA         NA        
    #> ASV8  "D"        ""        ""        ""         ""         ""        
    #> ASV12 NA         NA        NA        NA         NA         NA        
    #>       taxref_REU taxref_SA taxref_TA taxref_TAAF taxref_PF taxref_NC taxref_WF
    #> ASV7  NA         NA        NA        NA          NA        NA        NA       
    #> ASV8  ""         ""        ""        ""          ""        "P"       ""       
    #> ASV12 NA         NA        NA        NA          NA        NA        NA       
    #>       taxref_CLI taxref_URL                                     
    #> ASV7  NA         NA                                             
    #> ASV8  ""         "https://taxref.mnhn.fr/taxref-web/taxa/900725"
    #> ASV12 NA         NA                                             
    #>       taxref_URL_INPN                             taxref_NOM_VALIDE_SIMPLE
    #> ASV7  NA                                          NA                      
    #> ASV8  "https://inpn.mnhn.fr/espece/cd_nom/900725" "Stereum ostrea"        
    #> ASV12 NA                                          NA                      
    #>       Domain  Phylum          Class            Order            
    #> ASV7  "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"     
    #> ASV8  "Fungi" "Basidiomycota" "Agaricomycetes" "Russulales"     
    #> ASV12 "Fungi" "Basidiomycota" "Agaricomycetes" "Hymenochaetales"
    #>       Family           Genus     Species      Trophic.Mode
    #> ASV7  "Stereaceae"     NA        NA           "Saprotroph"
    #> ASV8  "Stereaceae"     "Stereum" "ostrea"     "Saprotroph"
    #> ASV12 "Schizoporaceae" "Xylodon" "raduloides" "Saprotroph"
    #>       Guild                                  Trait       Confidence.Ranking
    #> ASV7  "Wood Saprotroph-Undefined Saprotroph" "NULL"      "Probable"        
    #> ASV8  "Undefined Saprotroph"                 "White Rot" "Probable"        
    #> ASV12 "Undefined Saprotroph"                 "White Rot" "Probable"        
    #>       Genus_species        currentName                                
    #> ASV7  "NA_NA"              NA                                         
    #> ASV8  "Stereum_ostrea"     "Stereum ostrea (Blume & T.Nees) Fr., 1838"
    #> ASV12 "Xylodon_raduloides" "Xylodon (Pers.) Gray, 1821"               
    #>       currentCanonicalSimple genusEpithet specificEpithet genusSpeciesEpithet
    #> ASV7  NA                     NA           NA              NA                 
    #> ASV8  "Stereum ostrea"       "Stereum"    "ostrea"        "Stereum ostrea"   
    #> ASV12 NA                     "Xylodon"    NA              NA                 
    #>       namePublishedInYear authorship bracketauthorship scientificNameAuthorship
    #> ASV7  NA                  NA         NA                NA                      
    #> ASV8  "1838"              "Fr."      "Blume & T.Nees"  "(Blume & T.Nees) Fr."  
    #> ASV12 "1821"              "Gray"     "Pers."           "(Pers.) Gray"          
    #>       Global_occurences lang page_length page_views taxon_id   n_doi 
    #> ASV7  NA                NA   NA          NA         NA         NA    
    #> ASV8  " 11546"          " 9" "4397.556"  " 1007"    "Q2710042" " 126"
    #> ASV12 NA                NA   NA          NA         NA         NA    
    #>       target_taxon_name nb parasiteOf pathogenOf
    #> ASV7  NA                NA NA         NA        
    #> ASV8  NA                NA NA         NA        
    #> ASV12 NA                NA NA         NA

### Data Quality Assessment

Assess information completeness across sources:

``` r
# Calculate information completeness
completeness_analysis <- data_fungi_full@tax_table |>
  as.data.frame() |>
  summarise(
    gbif = 100 * mean(!is.na(Global_occurences)),
    wikipedia = 100 * mean(!is.na(lang)),
    globi = 100 * mean(!is.na(nb)),
    taxref = 100 * mean(!is.na(taxref_CD_NOM)),
    openalex = 100 * mean(!is.na(n_doi))
  ) |>
  tidyr::pivot_longer(everything(), names_to = "data_source", values_to = "completeness")
```

``` r
# Visualize data completeness
ggplot(completeness_analysis, aes(
  x = reorder(data_source, completeness),
  y = completeness, fill = data_source
)) +
  geom_col() +
  geom_hline(yintercept = 100) +
  coord_flip() +
  geom_label(aes(label = paste0(round(completeness, 1), "%"), y = completeness / 2), hjust = 0.5, col = "black", fill = rgb(1, 1, 1, 0.5)) +
  scale_fill_viridis_d() +
  labs(
    title = "Data Source Completeness",
    x = "Data Source",
    y = "Percentage of Taxa with Data"
  ) +
  theme_idest() +
  ylim(c(0, 100)) +
  theme(legend.position = "none")
```

![Horizontal bar chart showing data completeness percentages for
different external data sources (GBIF, Wikipedia, GLOBI, TAXREF,
OpenAlex). Each bar shows the percentage of taxa with available data
from that source, with percentage labels in the middle of each
bar.](figures/unnamed-chunk-18-1.png)

plot of chunk unnamed-chunk-18

### Integration Visualization

Create comprehensive visualization of integrated data:

⚠

×

**Warnings**

``` popup-pre
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `globi_nb_interactions = as.numeric(nb)`.
#> Caused by warning:
#> ! NAs introduits lors de la conversion automatique
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text_repel()`).
```

``` r
# Prepare data for visualization
viz_data <- data_fungi_full@tax_table |>
  as.data.frame() |>
  mutate(taxref_FR = tidyr::replace_na(taxref_FR, "")) |>
  mutate(
    Global_occurences = as.numeric(Global_occurences),
    wk_sum_page_views = as.numeric(page_views),
    globi_nb_interactions = as.numeric(nb),
    oa_n_doi = as.numeric(n_doi),
    taxref = taxref_FR != ""
  ) |>
  filter(!is.na(Global_occurences) | is.na(wk_sum_page_views)) |>
  distinct(currentCanonicalSimple, .keep_all = TRUE)
```

``` r

# Multi-dimensional visualization
ggplot(viz_data, aes(
  x = log10(Global_occurences + 1),
  y = log10(wk_sum_page_views + 1)
)) +
  geom_point(
    aes(
      size = log10(1 + as.numeric(oa_n_doi)),
      shape = taxref,
      color = Order
    ),
    alpha = 0.8
  ) +
  scale_shape_manual(values = c(17, 16), name = "Presence in France") +
  scale_color_viridis_d(name = "Order") +
  labs(
    title = "Integrated Taxonomic Information",
    subtitle = "GBIF occurrences, Wikipedia popularity, interactions, and traits",
    x = "GBIF Occurrences (log10)",
    y = "Wikipedia Page Views (log10)",
    size = "n_doi (log10)"
  ) +
  ggrepel::geom_text_repel(aes(label = currentCanonicalSimple), size = 3, fontface = "italic") +
  theme_idest()
```

![Scatter plot showing GBIF occurrences (x-axis, log scale) versus
Wikipedia page views (y-axis, log scale). Points are colored by Order,
shaped by presence in France (TAXREF), and sized by number of
publications (log scale).](figures/unnamed-chunk-19-1.png)

plot of chunk unnamed-chunk-19

### Knowledge Gap Identification

Identify taxa with limited information:

``` r
# Identify knowledge gaps
knowledge_gaps <- data_fungi_full@tax_table |>
  as.data.frame() |>
  select(Global_occurences, lang, nb, taxref_CD_NOM, n_doi, currentCanonicalSimple) |>
  mutate(
    gbif_info = !is.na(Global_occurences),
    wk_info = !is.na(lang),
    oa_info = !is.na(n_doi),
    taxref_info = (!is.na(taxref_CD_NOM) | taxref_CD_NOM == "NA"),
    globi_info = (!is.na(nb))
  ) |>
  mutate(taxref_info = ifelse(is.na(taxref_info), FALSE, taxref_info)) |>
  mutate(
    info_score = gbif_info + wk_info + oa_info + taxref_info + globi_info
  ) |>
  arrange(info_score)

# Number of available sources of information per taxa (ASV here)
knowledge_gaps |>
  pull(info_score) |>
  table()
```

    #> 
    #>  0  3  4  5 
    #> 17  1 16 11

``` r


# Number of available sources of information per taxonomic names (current canonical simple)
knowledge_gaps |>
  distinct(currentCanonicalSimple, .keep_all = TRUE) |>
  pull(info_score) |>
  table()
```

    #> 
    #>  0  3  4  5 
    #>  1  1 11  7

``` r

# Poorly knowns taxonomic names (current canonical simple)
knowledge_gaps |>
  distinct(currentCanonicalSimple, .keep_all = TRUE) |>
  filter(info_score <= 2) |>
  pull(currentCanonicalSimple)
```

    #> [1] NA

``` r
knowledge_gaps |>
  distinct(currentCanonicalSimple, .keep_all = TRUE) |>
  filter(!is.na(currentCanonicalSimple)) |>
  ComplexUpset::upset(
    intersect = c("gbif_info", "wk_info", "oa_info", "taxref_info", "globi_info"),
    keep_empty_groups = TRUE,
    wrap = TRUE, set_sizes = F
  ) +
  labs(
    title = "Knowledge Gaps Across Data Sources",
    subtitle = "Number of taxa names with available data from each source and their intersections"
  ) + theme_idest()
```

![UpSet plot showing intersections of data availability across five
information sources (GBIF, Wikipedia, OpenAlex, TAXREF, GLOBI).
Horizontal bars show the size of each data source, and vertical bars
show the size of unique combinations of sources for different
taxa.](figures/unnamed-chunk-21-1.png)

plot of chunk unnamed-chunk-21

## Best Practices

When integrating external data, consider the following best
practices: 1. **Taxonomic matching**: Ensure consistent naming across
sources 2. **Data validation**: Check for outliers or errors 3.
**Missing data**: Handle NAs appropriately in analyses 4.
**Documentation**: Keep track of data sources and versions

This comprehensive approach to external data integration transforms
basic taxonomic lists into rich, multi-dimensional datasets suitable for
advanced ecological analyses.

## Session information

``` r
sessionInfo()
```

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Pop!_OS 24.04 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
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
    #>  [1] ggraph_2.2.2            lubridate_1.9.5         forcats_1.0.1          
    #>  [4] stringr_1.6.0           purrr_1.2.2             readr_2.2.0            
    #>  [7] tidyr_1.3.2             tibble_3.3.1            tidyverse_2.0.0        
    #> [10] taxinfo_0.2.0.9000      MiscMetabar_0.17.0.9000 dplyr_1.2.1            
    #> [13] ggplot2_4.0.3           phyloseq_1.56.0        
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] Rdpack_2.6.6          bitops_1.0-9          gridExtra_2.3        
    #>   [4] permute_0.9-10        rlang_1.2.0           magrittr_2.0.5       
    #>   [7] ade4_1.7-24           otel_0.2.0            compiler_4.6.1       
    #>  [10] mgcv_1.9-4            openalexR_3.0.1       systemfonts_1.3.2    
    #>  [13] vctrs_0.7.3           reshape2_1.4.5        rgbif_3.8.5          
    #>  [16] httpcode_0.3.0        fastmap_1.2.0         pkgconfig_2.0.3      
    #>  [19] crayon_1.5.3          taxize_0.10.1         XVector_0.52.0       
    #>  [22] labeling_0.4.3        tzdb_0.5.0            bit_4.6.0            
    #>  [25] xfun_0.58             cachem_1.1.0          jsonlite_2.0.0       
    #>  [28] biomformat_1.40.0     tweenr_2.0.3          parallel_4.6.1       
    #>  [31] cluster_2.1.8.2       R6_2.6.1              stringi_1.8.7        
    #>  [34] RColorBrewer_1.1-3    ComplexUpset_1.3.3    Rcpp_1.1.1-1.1       
    #>  [37] Seqinfo_1.2.0         iterators_1.0.14      knitr_1.51           
    #>  [40] zoo_1.8-15            triebeard_0.4.1       IRanges_2.46.0       
    #>  [43] Matrix_1.7-5          splines_4.6.1         igraph_2.3.2         
    #>  [46] timechange_0.4.0      tidyselect_1.2.1      viridis_0.6.5        
    #>  [49] vegan_2.7-5           codetools_0.2-20      curl_7.1.0           
    #>  [52] lattice_0.22-9        plyr_1.8.9            Biobase_2.72.0       
    #>  [55] withr_3.0.2           S7_0.2.2              evaluate_1.0.5       
    #>  [58] survival_3.8-6        polyclip_1.10-7       RcppParallel_5.1.11-2
    #>  [61] xml2_1.5.2            Biostrings_2.80.1     pillar_1.11.1        
    #>  [64] WikipediR_1.7.1       whisker_0.4.1         foreach_1.5.2        
    #>  [67] stats4_4.6.1          generics_0.1.4        vroom_1.7.1          
    #>  [70] RCurl_1.98-1.19       S4Vectors_0.50.1      hms_1.1.4            
    #>  [73] scales_1.4.0          glue_1.8.1            lazyeval_0.2.3       
    #>  [76] tools_4.6.1           data.table_1.18.4     divent_0.5-4         
    #>  [79] graphlayouts_1.2.3    tidygraph_1.3.1       grid_4.6.1           
    #>  [82] ape_5.8-1             rbibutils_2.4.1       urltools_1.7.3.1     
    #>  [85] colorspace_2.1-2      patchwork_1.3.2       nlme_3.1-169         
    #>  [88] ggforce_0.5.0         cli_3.6.6             wikitaxa_0.5.0       
    #>  [91] viridisLite_0.4.3     gtable_0.3.6          oai_0.4.0            
    #>  [94] digest_0.6.39         BiocGenerics_0.58.1   rglobi_0.3.4         
    #>  [97] ggrepel_0.9.8         crul_1.6.0            farver_2.1.2         
    #> [100] memoise_2.0.1         multtest_2.68.0       lifecycle_1.0.5      
    #> [103] httr_1.4.8            bit64_4.8.2           MASS_7.3-65
