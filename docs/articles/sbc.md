# Post-clustering using taxonomic names

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

Riley *et al.* 2025 (<https://doi.org/10.1186/s12915-025-02284-x>)
propose the concept of species bound cluster (SBC) which are defined as
“clusters that include all and only ESVs assigned to one species, the
sequence similarity threshold can vary between these clusters”.

🛈

×

**Messages**

``` popup-pre
#> Cleaning suppress 0 taxa (  ) and 0 sample(s) (  ).
#> Number of non-matching ASV 0
#> Number of matching ASV 1420
#> Number of filtered-out ASV 86
#> Number of kept ASV 1334
#> Number of kept samples 185
```

``` r

d_f_ab <- subset_taxa_pq(data_fungi, taxa_sums(data_fungi) > 50)
```

## Compute intra-taxonomic names distance

``` r

intra_taxn_dist <- intra_taxnames_dist(d_f_ab, verbose = FALSE)

intra_taxn_dist |>
  filter(n_taxa > 1) |>
  arrange(desc(n_taxa)) |>
  select(-taxnames) |>
  head(10)
```

    #>                            n_taxa mean_dist    min_dist  max_dist
    #> Hyperphyscia adglutinata       15 0.1853030 0.003300330 0.3940397
    #> Dacrymyces stenosporus         14 0.2673166 0.003278689 0.4824281
    #> Eutypa spinosa                 13 0.1797748 0.006389776 0.3079470
    #> Stereum ostrea                 13 0.1636422 0.005602241 0.4068966
    #> Scytalidium lignicola          13 0.2563321 0.003205128 0.4700315
    #> Penicillium brevicompactum     13 0.2586561 0.003257329 0.4899329
    #> Scheffersomyces lignosus       11 0.2536107 0.005882353 0.4980843
    #> Angustimassarina acerina       10 0.2943042 0.003389831 0.4691358
    #> Xylodon raduloides              9 0.2416071 0.003030303 0.3971631
    #> Pertusaria leioplaca            9 0.3217032 0.005602241 0.4166667

## Cluster taxa into SBC

🛈

×

**Messages**

``` popup-pre
#> Compute the number of sequences
#> Cleaning suppress 0 taxa (  ) and 0 sample(s) (  ).
#> Number of non-matching ASV 0
#> Number of matching ASV 1420
#> Number of filtered-out ASV 1000
#> Number of kept ASV 420
#> Number of kept samples 185
#> Start object of class: phyloseq
#> Start object of class: phyloseq
#> Start object of class: phyloseq
#> Compute the number of clusters
#> Start object of class: phyloseq
#> Start object of class: phyloseq
#> Start object of class: phyloseq
#> Compute the number of samples
#> Start object of class: phyloseq
#> Start object of class: phyloseq
#> Start object of class: phyloseq
```

``` r

sbc_clusters <- cluster_sbc(d_f_ab, verbose = FALSE)

track_wkflow(list(
  "Raw physeq" = data_fungi,
  "Abundant taxa" = subset_taxa_pq(data_fungi, taxa_sums(data_fungi) > 500),
  "SBC" = sbc_clusters$physeq_SBC
)) |>
  knitr::kable()
```

    <!-- KNITR_ASIS_OUTPUT_TOKEN -->

    |              | nb_sequences| nb_clusters| nb_samples|
    |:-------------|------------:|-----------:|----------:|
    |Raw physeq    |      1839124|        1420|        185|
    |Abundant taxa |      1660524|         420|        185|
    |SBC           |       345218|         135|        178|

    <!-- KNITR_ASIS_OUTPUT_TOKEN -->```


    ``` r

    sbc_clusters$summary

    #>   n_taxnames n_taxa n_unassigned n_already_SBC n_taxa_to_cluster
    #> 1        252   1334          720           147               467
    #>   avg_cluster_size avg_cluster_size_excluding_singleton n_new_SBC n_SBC mean_d
    #> 1         2.177305                             3.459259       135   282    3.2
    #>   median_d
    #> 1        3

``` r

summary_plot_pq(sbc_clusters$physeq_SBC)
```

![Summary visualization showing the distribution and characteristics of
Species Bound Clusters (SBC) in the phyloseq object, including number of
taxa, samples, and other key metrics.](figures/unnamed-chunk-5-1.png)

## Visualize results

``` r

left_join(intra_taxn_dist, sbc_clusters$d_per_taxnames, by = "taxnames") |>
  filter(!is.na(optimal_d)) |>
  ggplot(aes(x = optimal_d, y = mean_dist, size = n_taxa.x, label = taxnames, color = other_taxnames == "")) +
  geom_point() +
  ggrepel::geom_text_repel(
    size = 2,
    force = 4,
    fontface = "italic"
  ) +
  scale_size_continuous(name = "Number of taxa", range = c(2, 4)) +
  # geom_hline(yintercept = 0.03, linetype = "dashed") +
  # annotate("label", label= "3%", x=5, y=0.03) +
  scale_color_manual(values = c("TRUE" = "gray40", "FALSE" = "seagreen4")) +
  ggrepel::geom_label_repel(aes(label = other_taxnames),
    size = 2,
    hjust = 1.4,
    fontface = "italic"
  ) +
  labs(
    x = "Optimal clustering swarm parameter d",
    y = "Mean intra-taxonomic names distance",
    color = "Other taxnames\n in the cluster",
    title = "An increasing optimal d captures taxonomic names\nwith higher intra-names distance",
    subtitle = "The squared names are other taxonomic names present in a cluster"
  ) +
  theme_idest()
```

![Scatter plot showing the relationship between optimal clustering swarm
parameter d (x-axis) and mean intra-taxonomic names distance (y-axis).
Points are sized by number of taxa and colored by whether other
taxonomic names are present in the
cluster.](figures/unnamed-chunk-6-1.png)

⚠

×

**Warnings**

``` popup-pre
#> Warning:  [1m [22mThe following aesthetics were dropped during statistical transformation:
#>  [32mcolour [39m.
#>  [36mℹ [39m This can happen when ggplot fails to infer the correct grouping structure in
#>   the data.
#>  [36mℹ [39m Did you forget to specify a `group` aesthetic or to convert a numerical
#>   variable into a factor?
```

``` r

left_join(intra_taxn_dist, sbc_clusters$d_per_taxnames, by = "taxnames") |>
  mutate(is_na_optimal_d = is.na(optimal_d)) |>
  filter(!is.na(mean_dist)) |>
  ggplot(aes(x = mean_dist, y = is_na_optimal_d, color = optimal_d)) +
  geom_violin() +
  geom_jitter() +
  geom_vline(xintercept = 0.03, color = "red") +
  theme_idest()
```

![Violin plot with jittered points showing the distribution of mean
intra-taxonomic distance (x-axis) grouped by whether optimal d is NA
(y-axis). Points are colored by optimal d value. A red vertical line
marks the threshold at 0.03.](figures/unnamed-chunk-7-1.png)

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
    #>   [1] DBI_1.2.3                   bitops_1.0-9               
    #>   [3] pbapply_1.7-4               deldir_2.0-4               
    #>   [5] permute_0.9-8               rlang_1.1.6                
    #>   [7] magrittr_2.0.4              ade4_1.7-23                
    #>   [9] matrixStats_1.5.0           compiler_4.5.1             
    #>  [11] mgcv_1.9-4                  png_0.1-8                  
    #>  [13] systemfonts_1.3.1           vctrs_0.6.5                
    #>  [15] reshape2_1.4.5              stringr_1.6.0              
    #>  [17] pwalign_1.6.0               pkgconfig_2.0.3            
    #>  [19] crayon_1.5.3                fastmap_1.2.0              
    #>  [21] XVector_0.50.0              labeling_0.4.3             
    #>  [23] Rsamtools_2.26.0            rmarkdown_2.30             
    #>  [25] ragg_1.5.0                  xfun_0.54                  
    #>  [27] cachem_1.1.0                cigarillo_1.0.0            
    #>  [29] jsonlite_2.0.0              biomformat_1.38.0          
    #>  [31] rhdf5filters_1.22.0         DelayedArray_0.36.0        
    #>  [33] Rhdf5lib_1.32.0             BiocParallel_1.44.0        
    #>  [35] jpeg_0.1-11                 parallel_4.5.1             
    #>  [37] cluster_2.1.8.1             R6_2.6.1                   
    #>  [39] bslib_0.9.0                 stringi_1.8.7              
    #>  [41] RColorBrewer_1.1-3          GenomicRanges_1.62.0       
    #>  [43] jquerylib_0.1.4             Seqinfo_1.0.0              
    #>  [45] SummarizedExperiment_1.40.0 iterators_1.0.14           
    #>  [47] knitr_1.50                  DECIPHER_3.6.0             
    #>  [49] IRanges_2.44.0              Matrix_1.7-4               
    #>  [51] splines_4.5.1               igraph_2.2.1               
    #>  [53] tidyselect_1.2.1            rstudioapi_0.17.1          
    #>  [55] abind_1.4-8                 yaml_2.3.10                
    #>  [57] vegan_2.7-2                 codetools_0.2-20           
    #>  [59] hwriter_1.3.2.1             lattice_0.22-7             
    #>  [61] tibble_3.3.0                plyr_1.8.9                 
    #>  [63] Biobase_2.70.0              withr_3.0.2                
    #>  [65] ShortRead_1.68.0            S7_0.2.1                   
    #>  [67] evaluate_1.0.5              desc_1.4.3                 
    #>  [69] survival_3.8-3              RcppParallel_5.1.11-1      
    #>  [71] Biostrings_2.78.0           pillar_1.11.1              
    #>  [73] MatrixGenerics_1.22.0       foreach_1.5.2              
    #>  [75] stats4_4.5.1                generics_0.1.4             
    #>  [77] RCurl_1.98-1.17             S4Vectors_0.48.0           
    #>  [79] scales_1.4.0                glue_1.8.0                 
    #>  [81] tools_4.5.1                 interp_1.1-6               
    #>  [83] data.table_1.17.8           GenomicAlignments_1.46.0   
    #>  [85] fs_1.6.6                    rhdf5_2.54.0               
    #>  [87] grid_4.5.1                  ape_5.8-1                  
    #>  [89] latticeExtra_0.6-31         tidyverse_2.0.0            
    #>  [91] nlme_3.1-168                cli_3.6.5                  
    #>  [93] textshaping_1.0.4           S4Arrays_1.10.0            
    #>  [95] gtable_0.3.6                sass_0.4.10                
    #>  [97] digest_0.6.38               BiocGenerics_0.56.0        
    #>  [99] ggrepel_0.9.6               rglobi_0.3.4               
    #> [101] SparseArray_1.10.1          htmlwidgets_1.6.4          
    #> [103] farver_2.1.2                htmltools_0.5.8.1          
    #> [105] pkgdown_2.2.0               multtest_2.66.0            
    #> [107] lifecycle_1.0.4             MASS_7.3-65
