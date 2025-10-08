
# taxinfo <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/adrientaudiere/taxinfo/workflows/R-CMD-check/badge.svg)](https://github.com/adrientaudiere/taxinfo/actions)
[![Codecov test
coverage](https://codecov.io/gh/adrientaudiere/taxinfo/branch/master/graph/badge.svg)](https://app.codecov.io/gh/adrientaudiere/taxinfo?branch=master)
[![CRAN
status](https://www.r-project.org/Badges/version/taxinfo)](https://CRAN.R-project.org/package=taxinfo)
<!-- badges: end -->

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

You can install the development version of taxinfo from GitHub:

``` r
# Install from GitHub
devtools::install_github("adrientaudiere/taxinfo")
#> Downloading GitHub repo adrientaudiere/taxinfo@HEAD
#> openssl     (2.3.3  -> 2.3.4 ) [CRAN]
#> rmarkdown   (2.29   -> 2.30  ) [CRAN]
#> systemfonts (1.2.3  -> 1.3.1 ) [CRAN]
#> forcats     (1.0.0  -> 1.0.1 ) [CRAN]
#> MiscMetabar (0.14.3 -> 0.14.4) [CRAN]
#> Skipping 2 packages not available: dada2, phyloseq
#> Installing 5 packages: openssl, rmarkdown, systemfonts, forcats, MiscMetabar
#> Installation des packages dans '/tmp/Rtmp8QmloL/temp_libpath1e6e51dc0a42e'
#> (car 'lib' n'est pas spécifié)
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/tmp/RtmpuNFcT0/remotes2022c517b3cd/adrientaudiere-taxinfo-98be538/DESCRIPTION’ ... OK
#> * preparing ‘taxinfo’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> Omitted ‘LazyData’ from DESCRIPTION
#> * building ‘taxinfo_0.1.2.tar.gz’
#> Installation du package dans '/tmp/Rtmp8QmloL/temp_libpath1e6e51dc0a42e'
#> (car 'lib' n'est pas spécifié)

# Or using pak
pak::pkg_install("adrientaudiere/taxinfo")
#> ! Using bundled GitHub PAT. Please add your own PAT using `gitcreds::gitcreds_set()`.
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> 
#> → Will install 198 packages.
#> → Will update 1 package.
#> → All 199 packages (139.40 MB) are cached.
#> + abind                         1.4-8     [bld]
#> + ade4                          1.7-23    [bld][cmp]
#> + ape                           5.8-1     [bld][cmp]
#> + askpass                       1.2.1     [bld][cmp]
#> + assertthat                    0.2.1     [bld]
#> + backports                     1.5.0     [bld][cmp]
#> + base64enc                     0.1-3     [bld][cmp]
#> + BH                            1.87.0-1  [bld]
#> + Biobase                       2.68.0    [bld][cmp]
#> + BiocGenerics                  0.54.0    [bld]
#> + BiocParallel                  1.42.2    [bld][cmp]
#> + biomformat                    1.36.0    [bld]
#> + Biostrings                    2.76.0    [bld][cmp]
#> + bit                           4.6.0     [bld][cmp]
#> + bit64                         4.6.0-1   [bld][cmp]
#> + bitops                        1.0-9     [bld][cmp]
#> + blob                          1.2.4     [bld]
#> + broom                         1.0.10    [bld]
#> + bslib                         0.9.0     [bld]
#> + cachem                        1.1.0     [bld][cmp]
#> + callr                         3.7.6     [bld]
#> + cellranger                    1.1.0     [bld]
#> + checkmate                     2.3.3     [bld][cmp]
#> + classInt                      0.4-11    [bld][cmp]
#> + cli                           3.6.5     [bld][cmp]
#> + clipr                         0.8.0     [bld]
#> + colorspace                    2.1-2     [bld][cmp]
#> + conflicted                    1.2.0     [bld]
#> + cpp11                         0.5.2     [bld]
#> + crayon                        1.5.3     [bld]
#> + crul                          1.6.0     [bld]
#> + curl                          7.0.0     [bld][cmp]
#> + dada2                         1.36.0    [bld][cmp]
#> + data.table                    1.17.8    [bld][cmp]
#> + DBI                           1.2.3     [bld]
#> + dbplyr                        2.5.1     [bld]
#> + DelayedArray                  0.34.1    [bld][cmp]
#> + deldir                        2.0-4     [bld][cmp]
#> + digest                        0.6.37    [bld][cmp]
#> + dplyr                         1.1.4     [bld][cmp]
#> + dtplyr                        1.3.2     [bld]
#> + e1071                         1.7-16    [bld][cmp]
#> + evaluate                      1.0.5     [bld]
#> + farver                        2.1.2     [bld][cmp]
#> + fastmap                       1.2.0     [bld][cmp]
#> + fastmatch                     1.1-6     [bld][cmp]
#> + fontawesome                   0.5.3     [bld]
#> + foreach                       1.5.2     [bld]
#> + formatR                       1.14      [bld]
#> + Formula                       1.2-5     [bld]
#> + fs                            1.6.6     [bld][cmp]
#> + futile.logger                 1.4.3     [bld]
#> + futile.options                1.0.1     [bld]
#> + gargle                        1.6.0     [bld]
#> + generics                      0.1.4     [bld]
#> + GenomeInfoDb                  1.44.3    [bld]
#> + GenomeInfoDbData              1.2.14    [bld]
#> + GenomicAlignments             1.44.0    [bld][cmp]
#> + GenomicRanges                 1.60.0    [bld][cmp]
#> + ggplot2                       4.0.0     [bld]
#> + glue                          1.8.0     [bld][cmp]
#> + googledrive                   2.1.2     [bld]
#> + googlesheets4                 1.1.2     [bld]
#> + gridExtra                     2.3       [bld]
#> + gtable                        0.3.6     [bld]
#> + haven                         2.5.5     [bld][cmp]
#> + highr                         0.11      [bld]
#> + Hmisc                         5.2-3     [bld][cmp]
#> + hms                           1.1.3     [bld]
#> + htmlTable                     2.4.3     [bld]
#> + htmltools                     0.5.8.1   [bld][cmp]
#> + htmlwidgets                   1.6.4     [bld]
#> + httpcode                      0.3.0     [bld]
#> + httr                          1.4.7     [bld]
#> + hwriter                       1.3.2.1   [bld]
#> + ids                           1.0.1     [bld]
#> + igraph                        2.1.4     [bld][cmp]
#> + interp                        1.1-6     [bld][cmp]
#> + IRanges                       2.42.0    [bld][cmp]
#> + isoband                       0.2.7     [bld][cmp]
#> + iterators                     1.0.14    [bld]
#> + jpeg                          0.1-11    [bld][cmp]
#> + jquerylib                     0.1.4     [bld]
#> + jsonlite                      2.0.0     [bld][cmp]
#> + knitr                         1.50      [bld]
#> + labeling                      0.4.3     [bld]
#> + lambda.r                      1.2.4     [bld]
#> + latticeExtra                  0.6-31    [bld]
#> + lazyeval                      0.2.2     [bld][cmp]
#> + lifecycle                     1.0.4     [bld]
#> + lubridate                     1.9.4     [bld][cmp]
#> + magrittr                      2.0.4     [bld][cmp]
#> + MatrixGenerics                1.20.0    [bld]
#> + matrixStats                   1.5.0     [bld][cmp]
#> + memoise                       2.0.1     [bld]
#> + mime                          0.13      [bld][cmp]
#> + modelr                        0.1.11    [bld]
#> + multtest                      2.64.0    [bld][cmp]
#> + natserv                       1.0.0     [bld]
#> + oai                           0.4.0     [bld]
#> + pbapply                       1.7-4     [bld]
#> + permute                       0.9-8     [bld]
#> + phangorn                      2.12.1    [bld][cmp]
#> + phyloseq                      1.52.0    [bld]
#> + pillar                        1.11.1    [bld]
#> + pixmap                        0.4-14    [bld]
#> + pkgconfig                     2.0.3     [bld]
#> + plyr                          1.8.9     [bld][cmp]
#> + png                           0.1-8     [bld][cmp]
#> + prettyunits                   1.2.0     [bld]
#> + processx                      3.8.6     [bld][cmp]
#> + progress                      1.2.3     [bld]
#> + proxy                         0.4-27    [bld][cmp]
#> + ps                            1.9.1     [bld][cmp]
#> + purrr                         1.1.0     [bld][cmp]
#> + pwalign                       1.4.0     [bld][cmp]
#> + quadprog                      1.5-8     [bld][cmp]
#> + R6                            2.6.1     [bld]
#> + ragg                          1.5.0     [bld][cmp]
#> + rappdirs                      0.3.3     [bld][cmp]
#> + ratelimitr                    0.4.1     [bld]
#> + RColorBrewer                  1.1-3     [bld]
#> + Rcpp                          1.1.0     [bld][cmp]
#> + RcppArmadillo                 15.0.2-2  [bld][cmp]
#> + RcppEigen                     0.3.4.0.2 [bld][cmp]
#> + RcppParallel                  5.1.11-1  [bld][cmp]
#> + readr                         2.1.5     [bld][cmp]
#> + readxl                        1.4.5     [bld][cmp]
#> + rematch                       2.0.0     [bld]
#> + rematch2                      2.1.2     [bld]
#> + rentrez                       1.2.4     [bld]
#> + reprex                        2.1.1     [bld]
#> + reshape2                      1.4.4     [bld][cmp]
#> + rex                           1.2.1     [bld]
#> + rgbif                         3.8.3     [bld]
#> + rhdf5                         2.52.1    [bld][cmp]
#> + rhdf5filters                  1.20.0    [bld][cmp]
#> + Rhdf5lib                      1.30.0    [bld][cmp]
#> + Rhtslib                       3.4.0     [bld][cmp]
#> + ritis                         1.0.0     [bld]
#> + rlang                         1.1.6     [bld][cmp]
#> + rncl                          0.8.7     [bld][cmp]
#> + rotl                          3.1.0     [bld]
#> + rredlist                      1.1.1     [bld]
#> + Rsamtools                     2.24.1    [bld][cmp]
#> + rstudioapi                    0.17.1    [bld]
#> + rvest                         1.0.5     [bld]
#> + s2                            1.1.9     [bld][cmp]
#> + S4Arrays                      1.8.1     [bld][cmp]
#> + S4Vectors                     0.46.0    [bld][cmp]
#> + S7                            0.2.0     [bld][cmp]
#> + sass                          0.4.10    [bld][cmp]
#> + scales                        1.4.0     [bld]
#> + selectr                       0.4-2     [bld]
#> + sf                            1.0-21    [bld][cmp]
#> + ShortRead                     1.66.0    [bld][cmp]
#> + snow                          0.4-4     [bld]
#> + solrium                       1.2.0     [bld]
#> + sp                            2.2-0     [bld][cmp]
#> + SparseArray                   1.8.1     [bld][cmp]
#> + stringi                       1.8.7     [bld][cmp]
#> + stringr                       1.5.2     [bld]
#> + SummarizedExperiment          1.38.1    [bld]
#> + sys                           3.4.3     [bld][cmp]
#> + taxinfo               0.1.2 → 0.1.2     [bld][cmp] (GitHub: 6f1a92e)
#> + taxize                        0.10.0    [bld]
#> + textshaping                   1.0.3     [bld][cmp]
#> + tibble                        3.3.0     [bld][cmp]
#> + tidyr                         1.3.1     [bld][cmp]
#> + tidyselect                    1.2.1     [bld][cmp]
#> + tidyverse                     2.0.0     [bld]
#> + timechange                    0.3.0     [bld][cmp]
#> + tinytex                       0.57      [bld]
#> + triebeard                     0.4.1     [bld][cmp]
#> + tzdb                          0.5.0     [bld][cmp]
#> + UCSC.utils                    1.4.0     [bld]
#> + units                         0.8-7     [bld][cmp]
#> + urltools                      1.7.3.1   [bld][cmp]
#> + utf8                          1.2.6     [bld][cmp]
#> + uuid                          1.2-1     [bld][cmp]
#> + vctrs                         0.6.5     [bld][cmp]
#> + vegan                         2.7-1     [bld][cmp]
#> + viridis                       0.6.5     [bld]
#> + viridisLite                   0.4.2     [bld]
#> + vroom                         1.6.6     [bld][cmp]
#> + whisker                       0.4.1     [bld]
#> + WikidataQueryServiceR         1.0.0     [bld]
#> + WikidataR                     2.3.3     [bld]
#> + WikipediR                     1.7.1     [bld]
#> + wikitaxa                      0.4.0     [bld]
#> + withr                         3.0.2     [bld]
#> + wk                            0.9.4     [bld][cmp]
#> + worrms                        0.4.3     [bld]
#> + xfun                          0.53      [bld][cmp]
#> + XML                           3.99-0.19 [bld][cmp]
#> + xml2                          1.4.0     [bld][cmp]
#> + XVector                       0.48.0    [bld][cmp]
#> + yaml                          2.3.10    [bld][cmp]
#> + zoo                           1.8-14    [bld][cmp]
#> ℹ No downloads are needed, 199 pkgs (139.40 MB) are cached
#> ✔ Got taxinfo 0.1.2 (source) (57.70 MB)
#> ℹ Building Rhdf5lib 1.30.0
#> ℹ Building Rhtslib 3.4.0
#> ℹ Building GenomeInfoDbData 1.2.14
#> ✔ Installed abind 1.4-8  (61ms)
#> ✔ Installed ade4 1.7-23  (132ms)
#> ✔ Installed ape 5.8-1  (151ms)
#> ✔ Installed askpass 1.2.1  (108ms)
#> ✔ Installed assertthat 0.2.1  (65ms)
#> ✔ Installed backports 1.5.0  (94ms)
#> ✔ Installed base64enc 0.1-3  (71ms)
#> ✔ Installed bit 4.6.0  (54ms)
#> ✔ Installed bit64 4.6.0-1  (146ms)
#> ✔ Installed bitops 1.0-9  (74ms)
#> ✔ Installed blob 1.2.4  (107ms)
#> ✔ Installed broom 1.0.10  (105ms)
#> ✔ Installed bslib 0.9.0  (113ms)
#> ✔ Installed cachem 1.1.0  (66ms)
#> ✔ Installed callr 3.7.6  (69ms)
#> ✔ Installed BH 1.87.0-1  (722ms)
#> ✔ Installed cellranger 1.1.0  (132ms)
#> ✔ Installed checkmate 2.3.3  (106ms)
#> ✔ Installed classInt 0.4-11  (73ms)
#> ✔ Installed cli 3.6.5  (97ms)
#> ✔ Installed clipr 0.8.0  (68ms)
#> ✔ Installed colorspace 2.1-2  (70ms)
#> ✔ Built GenomeInfoDbData 1.2.14 (1.6s)
#> ✔ Installed conflicted 1.2.0  (138ms)
#> ✔ Installed cpp11 0.5.2  (111ms)
#> ✔ Installed crayon 1.5.3  (66ms)
#> ✔ Installed crul 1.6.0  (91ms)
#> ✔ Installed curl 7.0.0  (64ms)
#> ✔ Installed data.table 1.17.8  (70ms)
#> ✔ Installed DBI 1.2.3  (98ms)
#> ✔ Installed dbplyr 2.5.1  (94ms)
#> ✔ Installed deldir 2.0-4  (66ms)
#> ✔ Installed digest 0.6.37  (65ms)
#> ✔ Installed dplyr 1.1.4  (95ms)
#> ✔ Installed dtplyr 1.3.2  (67ms)
#> ✔ Installed e1071 1.7-16  (65ms)
#> ✔ Installed evaluate 1.0.5  (90ms)
#> ✔ Installed farver 2.1.2  (66ms)
#> ✔ Installed fastmap 1.2.0  (62ms)
#> ✔ Installed fastmatch 1.1-6  (57ms)
#> ✔ Installed fontawesome 0.5.3  (230ms)
#> ✔ Installed foreach 1.5.2  (62ms)
#> ✔ Installed formatR 1.14  (62ms)
#> ✔ Installed Formula 1.2-5  (80ms)
#> ✔ Installed fs 1.6.6  (79ms)
#> ✔ Installed futile.logger 1.4.3  (62ms)
#> ✔ Installed futile.options 1.0.1  (60ms)
#> ✔ Installed gargle 1.6.0  (87ms)
#> ✔ Installed generics 0.1.4  (62ms)
#> ℹ Building BiocGenerics 0.54.0
#> ✔ Installed ggplot2 4.0.0  (82ms)
#> ✔ Installed glue 1.8.0  (33ms)
#> ✔ Installed googledrive 2.1.2  (63ms)
#> ✔ Installed googlesheets4 1.1.2  (70ms)
#> ✔ Installed gridExtra 2.3  (66ms)
#> ✔ Installed gtable 0.3.6  (94ms)
#> ✔ Installed haven 2.5.5  (100ms)
#> ✔ Installed highr 0.11  (68ms)
#> ✔ Installed Hmisc 5.2-3  (88ms)
#> ✔ Installed hms 1.1.3  (64ms)
#> ✔ Installed htmlTable 2.4.3  (67ms)
#> ✔ Installed htmltools 0.5.8.1  (93ms)
#> ✔ Installed htmlwidgets 1.6.4  (91ms)
#> ✔ Installed httpcode 0.3.0  (67ms)
#> ✔ Installed httr 1.4.7  (66ms)
#> ✔ Installed hwriter 1.3.2.1  (94ms)
#> ✔ Installed ids 1.0.1  (67ms)
#> ✔ Installed isoband 0.2.7  (61ms)
#> ✔ Installed iterators 1.0.14  (43ms)
#> ✔ Installed igraph 2.1.4  (275ms)
#> ✔ Installed jpeg 0.1-11  (68ms)
#> ✔ Installed interp 1.1-6  (321ms)
#> ✔ Installed jquerylib 0.1.4  (120ms)
#> ✔ Installed jsonlite 2.0.0  (118ms)
#> ✔ Installed knitr 1.50  (82ms)
#> ✔ Installed labeling 0.4.3  (72ms)
#> ✔ Installed lambda.r 1.2.4  (84ms)
#> ✔ Installed latticeExtra 0.6-31  (73ms)
#> ✔ Installed lazyeval 0.2.2  (70ms)
#> ✔ Installed lifecycle 1.0.4  (96ms)
#> ✔ Installed lubridate 1.9.4  (72ms)
#> ✔ Installed magrittr 2.0.4  (71ms)
#> ✔ Installed matrixStats 1.5.0  (101ms)
#> ✔ Installed memoise 2.0.1  (79ms)
#> ℹ Building MatrixGenerics 1.20.0
#> ✔ Installed mime 0.13  (29ms)
#> ✔ Installed modelr 0.1.11  (34ms)
#> ✔ Installed natserv 1.0.0  (99ms)
#> ✔ Installed oai 0.4.0  (50ms)
#> ✔ Installed pbapply 1.7-4  (62ms)
#> ✔ Installed permute 0.9-8  (112ms)
#> ✔ Installed phangorn 2.12.1  (112ms)
#> ✔ Installed pillar 1.11.1  (83ms)
#> ✔ Installed pixmap 0.4-14  (91ms)
#> ✔ Installed pkgconfig 2.0.3  (88ms)
#> ✔ Installed plyr 1.8.9  (86ms)
#> ✔ Installed png 0.1-8  (97ms)
#> ✔ Installed prettyunits 1.2.0  (84ms)
#> ✔ Installed processx 3.8.6  (65ms)
#> ✔ Installed progress 1.2.3  (76ms)
#> ✔ Built BiocGenerics 0.54.0 (3.2s)
#> ✔ Installed proxy 0.4-27  (117ms)
#> ✔ Installed ps 1.9.1  (88ms)
#> ✔ Installed purrr 1.1.0  (70ms)
#> ✔ Installed quadprog 1.5-8  (87ms)
#> ✔ Installed R6 2.6.1  (64ms)
#> ✔ Installed rappdirs 0.3.3  (34ms)
#> ✔ Installed ratelimitr 0.4.1  (65ms)
#> ✔ Installed RColorBrewer 1.1-3  (94ms)
#> ✔ Installed ragg 1.5.0  (1.3s)
#> ✔ Installed Rcpp 1.1.0  (1.1s)
#> ✔ Installed RcppArmadillo 15.0.2-2  (1.1s)
#> ✔ Built MatrixGenerics 1.20.0 (2.7s)
#> ✔ Installed RcppParallel 5.1.11-1  (83ms)
#> ✔ Installed RcppEigen 0.3.4.0.2  (224ms)
#> ✔ Installed readr 2.1.5  (104ms)
#> ✔ Installed readxl 1.4.5  (99ms)
#> ✔ Installed rematch 2.0.0  (89ms)
#> ✔ Installed rematch2 2.1.2  (59ms)
#> ✔ Installed rentrez 1.2.4  (61ms)
#> ✔ Installed reprex 2.1.1  (91ms)
#> ✔ Installed reshape2 1.4.4  (65ms)
#> ✔ Installed rex 1.2.1  (61ms)
#> ✔ Installed rgbif 3.8.3  (88ms)
#> ✔ Installed ritis 1.0.0  (90ms)
#> ✔ Installed rlang 1.1.6  (66ms)
#> ✔ Installed rotl 3.1.0  (29ms)
#> ✔ Installed rredlist 1.1.1  (90ms)
#> ✔ Installed rncl 0.8.7  (204ms)
#> ✔ Installed rstudioapi 0.17.1  (97ms)
#> ✔ Installed rvest 1.0.5  (121ms)
#> ✔ Installed S7 0.2.0  (37ms)
#> ✔ Installed sass 0.4.10  (72ms)
#> ✔ Installed scales 1.4.0  (69ms)
#> ✔ Installed selectr 0.4-2  (96ms)
#> ✔ Installed snow 0.4-4  (29ms)
#> ℹ Building BiocParallel 1.42.2
#> ✔ Installed s2 1.1.9  (438ms)
#> ✔ Installed solrium 1.2.0  (184ms)
#> ✔ Installed sf 1.0-21  (307ms)
#> ✔ Installed sp 2.2-0  (91ms)
#> ✔ Installed stringi 1.8.7  (126ms)
#> ✔ Installed stringr 1.5.2  (144ms)
#> ✔ Installed sys 3.4.3  (91ms)
#> ✔ Installed taxize 0.10.0  (78ms)
#> ✔ Installed textshaping 1.0.3  (112ms)
#> ✔ Installed tibble 3.3.0  (134ms)
#> ✔ Installed tidyr 1.3.1  (105ms)
#> ✔ Installed tidyselect 1.2.1  (94ms)
#> ✔ Installed tidyverse 2.0.0  (70ms)
#> ✔ Installed timechange 0.3.0  (71ms)
#> ✔ Installed tinytex 0.57  (95ms)
#> ✔ Installed triebeard 0.4.1  (69ms)
#> ✔ Installed tzdb 0.5.0  (71ms)
#> ✔ Installed units 0.8-7  (103ms)
#> ✔ Installed urltools 1.7.3.1  (102ms)
#> ✔ Installed utf8 1.2.6  (78ms)
#> ✔ Installed uuid 1.2-1  (68ms)
#> ✔ Installed vctrs 0.6.5  (106ms)
#> ✔ Installed vegan 2.7-1  (81ms)
#> ✔ Installed viridis 0.6.5  (76ms)
#> ✔ Installed viridisLite 0.4.2  (107ms)
#> ✔ Installed whisker 0.4.1  (33ms)
#> ✔ Installed vroom 1.6.6  (160ms)
#> ✔ Installed WikidataQueryServiceR 1.0.0  (134ms)
#> ✔ Installed WikidataR 2.3.3  (135ms)
#> ✔ Installed WikipediR 1.7.1  (73ms)
#> ✔ Installed wikitaxa 0.4.0  (75ms)
#> ✔ Installed withr 3.0.2  (108ms)
#> ✔ Installed wk 0.9.4  (81ms)
#> ✔ Installed worrms 0.4.3  (57ms)
#> ✔ Installed xfun 0.53  (65ms)
#> ✔ Installed XML 3.99-0.19  (98ms)
#> ✔ Installed xml2 1.4.0  (67ms)
#> ✔ Installed yaml 2.3.10  (67ms)
#> ✔ Installed zoo 1.8-14  (99ms)
#> ✔ Installed BiocGenerics 0.54.0  (84ms)
#> ℹ Building Biobase 2.68.0
#> ✔ Installed MatrixGenerics 1.20.0  (95ms)
#> ℹ Building S4Vectors 0.46.0
#> ✔ Installed GenomeInfoDbData 1.2.14  (1.2s)
#> ✔ Built Biobase 2.68.0 (7.1s)
#> ✔ Installed Biobase 2.68.0  (43ms)
#> ℹ Building multtest 2.64.0
#> ✔ Built BiocParallel 1.42.2 (12s)
#> ✔ Installed BiocParallel 1.42.2  (64ms)
#> ✔ Built multtest 2.64.0 (7.4s)
#> ✔ Installed multtest 2.64.0  (22ms)
#> ✔ Built S4Vectors 0.46.0 (17.3s)
#> ✔ Installed S4Vectors 0.46.0  (45ms)
#> ℹ Building IRanges 2.42.0
#> ℹ Building UCSC.utils 1.4.0
#> ✔ Built UCSC.utils 1.4.0 (2.3s)
#> ✔ Installed UCSC.utils 1.4.0  (26ms)
#> ✔ Built IRanges 2.42.0 (35.7s)
#> ✔ Installed IRanges 2.42.0  (39ms)
#> ℹ Building GenomeInfoDb 1.44.3
#> ℹ Building S4Arrays 1.8.1
#> ℹ Building XVector 0.48.0
#> ✔ Built Rhtslib 3.4.0 (1m 7.2s)
#> ✔ Installed Rhtslib 3.4.0  (157ms)
#> ✔ Built GenomeInfoDb 1.44.3 (6.9s)
#> ✔ Installed GenomeInfoDb 1.44.3  (54ms)
#> ✔ Built XVector 0.48.0 (8.5s)
#> ✔ Installed XVector 0.48.0  (24ms)
#> ℹ Building Biostrings 2.76.0
#> ℹ Building GenomicRanges 1.60.0
#> ✔ Built S4Arrays 1.8.1 (8.8s)
#> ✔ Installed S4Arrays 1.8.1  (73ms)
#> ℹ Building SparseArray 1.8.1
#> ✔ Built GenomicRanges 1.60.0 (12.8s)
#> ✔ Installed GenomicRanges 1.60.0  (39ms)
#> ✔ Built Biostrings 2.76.0 (19.7s)
#> ✔ Installed Biostrings 2.76.0  (110ms)
#> ℹ Building pwalign 1.4.0
#> ℹ Building Rsamtools 2.24.1
#> ✔ Built SparseArray 1.8.1 (20.9s)
#> ✔ Installed SparseArray 1.8.1  (52ms)
#> ℹ Building DelayedArray 0.34.1
#> ✔ Built pwalign 1.4.0 (9.4s)
#> ✔ Installed pwalign 1.4.0  (29ms)
#> ✔ Built DelayedArray 0.34.1 (17.9s)
#> ✔ Installed DelayedArray 0.34.1  (1.1s)
#> ℹ Building SummarizedExperiment 1.38.1
#> ✔ Built Rsamtools 2.24.1 (30.8s)
#> ✔ Installed Rsamtools 2.24.1  (119ms)
#> ✔ Built SummarizedExperiment 1.38.1 (14.9s)
#> ✔ Installed SummarizedExperiment 1.38.1  (59ms)
#> ℹ Building GenomicAlignments 1.44.0
#> ✔ Built GenomicAlignments 1.44.0 (18.5s)
#> ✔ Installed GenomicAlignments 1.44.0  (42ms)
#> ℹ Building ShortRead 1.66.0
#> ✔ Built ShortRead 1.66.0 (26.6s)
#> ✔ Installed ShortRead 1.66.0  (63ms)
#> ℹ Building dada2 1.36.0
#> ✔ Built Rhdf5lib 1.30.0 (2m 55.4s)
#> ✔ Installed Rhdf5lib 1.30.0  (300ms)
#> ℹ Building rhdf5filters 1.20.0
#> ✔ Built rhdf5filters 1.20.0 (9.5s)
#> ✔ Installed rhdf5filters 1.20.0  (27ms)
#> ℹ Building rhdf5 2.52.1
#> ✔ Built rhdf5 2.52.1 (10.1s)
#> ✔ Installed rhdf5 2.52.1  (118ms)
#> ℹ Building biomformat 1.36.0
#> ✔ Built biomformat 1.36.0 (3.3s)
#> ✔ Installed biomformat 1.36.0  (23ms)
#> ℹ Building phyloseq 1.52.0
#> ✔ Built phyloseq 1.52.0 (13.7s)
#> ✔ Installed phyloseq 1.52.0  (75ms)
#> ✔ Built dada2 1.36.0 (48.8s)
#> ✔ Installed dada2 1.36.0  (81ms)
#> ℹ Packaging taxinfo 0.1.2
#> ✔ Packaged taxinfo 0.1.2 (10.4s)
#> ℹ Building taxinfo 0.1.2
#> ✔ Built taxinfo 0.1.2 (28.5s)
#> ✔ Installed taxinfo 0.1.2 (github::adrientaudiere/taxinfo@6f1a92e) (654ms)
#> ✔ 1 pkg + 216 deps: kept 17, upd 1, added 198, dld 1 (NA B) [5m 37s]
```

## Key Features

### 🔍 **Data Verification & Quality Control**

- `gna_verifier_pq()`: Verify and standardize taxonomic names using
  Global Names Architecture

### 🌍 **Biodiversity Data Integration**

- `tax_gbif_occur_pq()`: Retrieve GBIF occurrence data
- `tax_globi_pq()`: Access species interaction data from GLOBI
- `tax_info_pq()`: Add information from CSV files (TAXREF, traits
  databases)

### 📚 **Knowledge Base Integration**

- `tax_get_wk_info_pq()`: Get comprehensive Wikipedia data
- `tax_oa_pq()`: Retrieve scientific literature from OpenAlex

### 🗺️ **Geographic Analysis**

- `range_bioreg_pq()`: Analyze biogeographic ranges
- `plot_tax_gbif_pq()`: Create distribution maps
- `tax_check_ecoregion()`: Validate occurrences against ecoregions

### 🔬 **Advanced Analysis Tools**

- `tax_retroblast_pq()`: Sequence-based taxonomic verification
- `tax_photos_pq()`: Access taxonomic images and media
- `tax_occur_check_pq()`: Multi-source occurrence validation

### 🎯 **Flexible Input Options**

Most functions can work with either: - **Phyloseq objects**:
Automatically enriches the tax_table (default behavior) - **Taxonomic
name vectors**: Returns tibbles for standalone queries

## Quick Start

``` r
library(taxinfo)
#> Le chargement a nécessité le package : MiscMetabar
#> Le chargement a nécessité le package : phyloseq
#> Le chargement a nécessité le package : ggplot2
#> Le chargement a nécessité le package : dada2
#> Le chargement a nécessité le package : Rcpp
#> Le chargement a nécessité le package : dplyr
#> 
#> Attachement du package : 'dplyr'
#> Les objets suivants sont masqués depuis 'package:stats':
#> 
#>     filter, lag
#> Les objets suivants sont masqués depuis 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Le chargement a nécessité le package : purrr
library(MiscMetabar)

# Load example data (fungal phyloseq object from MiscMetabar)
data("data_fungi_mini", package = "MiscMetabar")

# Step 1: Verify and clean taxonomic names
data_fungi_clean <- gna_verifier_pq(data_fungi_mini,
  data_sources = 210
)
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 45
#> • Taxa submitted for verification: 37
#> • Genus-level only taxa: 2
#> • Total matches found: 25
#> • Synonyms: 4 (including 4 at genus level)
#> • Accepted names: 21 (including 15 at genus level)

# Step 2: Add GBIF occurrence data (add_to_phyloseq defaults to TRUE)
data_with_gbif <- tax_gbif_occur_pq(data_fungi_clean, )
#> ℹ Processing GBIF occurrences for Stereum ostrea
#> ℹ Processing GBIF occurrences for Ossicaulis lachnopus
#> ■■■■■■                            17% | ETA:  6s
#> ℹ Processing GBIF occurrences for Stereum hirsutum
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Basidiodendron eyrei
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Sistotrema oblongisporum
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Fomes fomentarius
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Cerocorticium molare
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Aporpium canescens
#> ■■■■■■                            17% | ETA:  6sℹ Processing GBIF occurrences for Hypochnicium analogum
#> ■■■■■■                            17% | ETA:  6s■■■■■■■■■■■■■■■■■■                56% | ETA:  3s
#> ℹ Processing GBIF occurrences for Hyphoderma roseocremeum
#> ■■■■■■■■■■■■■■■■■■                56% | ETA:  3sℹ Processing GBIF occurrences for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■                56% | ETA:  3sℹ Processing GBIF occurrences for Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■                56% | ETA:  3sℹ Processing GBIF occurrences for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■                56% | ETA:  3sℹ Processing GBIF occurrences for Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■                56% | ETA:  3sℹ Processing GBIF occurrences for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■                56% | ETA:  3s■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  1s
#> ℹ Processing GBIF occurrences for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  1sℹ Processing GBIF occurrences for Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  1s■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ Processing GBIF occurrences for Xylodon flaviporus

# Step 3: Add trait information
fungal_traits <- system.file("extdata", "fun_trait_mini.csv", package = "taxinfo")
data_with_traits <- tax_info_pq(data_with_gbif,
  taxonomic_rank = "genus",
  file_name = fungal_traits,
  csv_taxonomic_rank = "GENUS",
  col_prefix = "ft_",
  sep = ";"
)

# Step 4: Add Wikipedia information (add_to_phyloseq defaults to TRUE)
data_final <- tax_get_wk_info_pq(data_with_traits)
#> ℹ Getting taxonomic IDs from Wikidata...
#> ℹ Getting page views from Wikipedia for Stereum ostrea
#> ■■■■                              11% | ETA: 48s
#> ℹ Getting page views from Wikipedia for Ossicaulis lachnopus
#> ■■■■                              11% | ETA: 48s■■■■■■                            16% | ETA: 44s
#> ℹ Getting page views from Wikipedia for Stereum hirsutum
#> ■■■■■■                            16% | ETA: 44s■■■■■■■                           21% | ETA:  1m
#> ℹ Getting page views from Wikipedia for Basidiodendron eyrei
#> ■■■■■■■                           21% | ETA:  1m■■■■■■■■■                         26% | ETA:  1m
#> ℹ Getting page views from Wikipedia for Sistotrema oblongisporum
#> ■■■■■■■■■                         26% | ETA:  1m■■■■■■■■■■                        32% | ETA:  1m
#> ℹ Getting page views from Wikipedia for Fomes fomentarius
#> ■■■■■■■■■■                        32% | ETA:  1m■■■■■■■■■■■■                      37% | ETA:  2m
#> ℹ Getting page views from Wikipedia for Mycena renatii
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Cerocorticium molare
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Aporpium canescens
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Hypochnicium analogum
#> ■■■■■■■■■■■■                      37% | ETA:  2mℹ Getting page views from Wikipedia for Hyphoderma roseocremeum
#> ■■■■■■■■■■■■                      37% | ETA:  2m■■■■■■■■■■■■■■■■■■■■              63% | ETA: 33s
#> ℹ Getting page views from Wikipedia for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 33s■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 27s
#> ℹ Getting page views from Wikipedia for Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 27s■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 30s
#> ℹ Getting page views from Wikipedia for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 30s■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 23s
#> ℹ Getting page views from Wikipedia for Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 23s■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 18s
#> ℹ Getting page views from Wikipedia for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 18s■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA: 12s
#> ℹ Getting page views from Wikipedia for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA: 12s■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  6s
#> ℹ Getting page views from Wikipedia for Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  6s■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ℹ Getting page views from Wikipedia for Xylodon flaviporus

# View the enriched taxonomic table
head(data_final@tax_table)
#> Taxonomy Table:     [6 taxa by 42 taxonomic ranks]:
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
#>       currentCanonicalSimple genus
#> ASV7  NA                     NA                         
#> ASV8  "Stereum ostrea"       "Stereum"                  
#> ASV12 "Xylodon"              "Xylodon"                  
#> ASV18 "Stereum ostrea"       "Stereum"                  
#> ASV25 "Ossicaulis lachnopus" "Ossicaulis"               
#> ASV26 "Stereum hirsutum"     "Stereum"                  
#>       specificEpithet Global_occurences taxa_name             
#> ASV7  NA                            NA                "NA"                  
#> ASV8  "ostrea"                      " 10196"          "Stereum ostrea"      
#> ASV12 NA                            NA                "Xylodon"             
#> ASV18 "ostrea"                      " 10196"          "Stereum ostrea"      
#> ASV25 "lachnopus"                   "   203"          "Ossicaulis lachnopus"
#> ASV26 "hirsutum"                    " 91303"          "Stereum hirsutum"    
#>       lang page_length page_views taxon_id   
#> ASV7  NA   NA          NA         NA         
#> ASV8  " 9" "4408.000"  " 1641"    "Q2710042" 
#> ASV12 NA   NA          NA         NA         
#> ASV18 " 9" "4408.000"  " 1641"    "Q2710042" 
#> ASV25 " 4" "2223.250"  "   30"    "Q10613125"
#> ASV26 "24" "5476.750"  " 2142"    "Q557377"

# Alternative: Query specific taxa without a phyloseq object
taxa_info <- tax_gbif_occur_pq(taxnames = c("Amanita muscaria", "Boletus edulis"), by_country = TRUE)
#> ℹ Processing GBIF occurrences for Amanita muscaria
#> ℹ Processing GBIF occurrences for Boletus edulis

# Returns a tibble instead of phyloseq object
head(taxa_info)
#> # A tibble: 6 × 3
#>   name   count canonicalName   
#>   <chr>  <int> <chr>           
#> 1 NL    107716 Amanita muscaria
#> 2 US     25415 Amanita muscaria
#> 3 GB     15599 Amanita muscaria
#> 4 DE     13636 Amanita muscaria
#> 5 SE      8453 Amanita muscaria
#> 6 DK      8190 Amanita muscaria
```

## Function Categories

### Data Sources Integration

Functions to connect with external databases and APIs: - GBIF (Global
Biodiversity Information Facility) - Wikipedia and Wikidata - GLOBI
(Global Biotic Interactions) - OpenAlex (Scientific Literature) - TAXREF
(French taxonomic reference) - Custom trait databases

### Analysis & Visualization

Tools for analyzing and visualizing taxonomic data: - Geographic
distribution analysis - Biogeographic range modeling - Occurrence
validation - Interactive mapping

### Utilities & Helpers

Supporting functions for data manipulation: - Taxonomic name
standardization - Data quality assessment - Phyloseq object
manipulation - Summary statistics

## Data Sources

**taxinfo** integrates with multiple authoritative data sources:

| Source | Description | Functions |
|----|----|----|
| **GBIF** | Global biodiversity occurrence data | `tax_gbif_occur_pq()`, `plot_tax_gbif_pq()` |
| **Wikipedia** | Encyclopedia data and page statistics | `tax_get_wk_info_pq()`, `tax_get_wk_pages_info()` |
| **GLOBI** | Species interaction networks | `tax_globi_pq()` |
| **OpenAlex** | Scientific literature database | `tax_oa_pq()` |
| **TAXREF** | French national taxonomic reference | `tax_info_pq()` |
| **GNA** | Global Names Architecture for name verification | `gna_verifier_pq()` |
| **Custom CSV** | Any taxonomic database in CSV format | `tax_info_pq()` |

## Performance Features

- **Memory efficient**: Use DuckDB for large dataset processing
- **Parallel processing**: Support for concurrent API calls
- **Caching**: Smart caching to avoid redundant API requests
- **Batch processing**: Efficient handling of multiple taxa

## Contributing

We welcome contributions! Please see our [Contributing
Guide](CONTRIBUTING.md) for details.

## Citation

If you use taxinfo in your research, please cite:

``` r
citation("taxinfo")
```

## Related Packages

**taxinfo** works seamlessly with: -
[MiscMetabar](https://github.com/adrientaudiere/MiscMetabar):
Miscellaneous functions for metabarcoding analysis -
[phyloseq](https://joey711.github.io/phyloseq/): Analyze microbiome
census data - [taxize](https://github.com/ropensci/taxize): Taxonomic
information from around the web -
[rgbif](https://github.com/ropensci/rgbif): Interface to GBIF API

## Licence

This project is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details.
