
# taxinfo

<!-- badges: start -->
[![R-CMD-check](https://github.com/adrientaudiere/taxinfo/workflows/R-CMD-check/badge.svg)](https://github.com/adrientaudiere/taxinfo/actions)
[![CRAN status](https://www.r-project.org/Badges/version/taxinfo)](https://CRAN.R-project.org/package=taxinfo)
<!-- badges: end -->

The **taxinfo** package provides comprehensive tools for augmenting phyloseq objects with taxonomic-based information from various external data sources including GBIF, Wikipedia, GLOBI, OpenAlex, TAXREF, and other databases.

## Installation

You can install the development version of taxinfo from GitHub:

``` r
# Install from GitHub
devtools::install_github("adrientaudiere/taxinfo")

# Or using pak
pak::pkg_install("adrientaudiere/taxinfo")
```

## Key Features

- **Data Verification**: Verify and standardize taxonomic names using Global Names Architecture
- **Biodiversity Data**: Access GBIF occurrence data and species interactions from GLOBI
- **Knowledge Integration**: Retrieve Wikipedia data and scientific literature from OpenAlex
- **Geographic Analysis**: Analyze biogeographic ranges and create distribution maps
- **Advanced Tools**: Sequence-based verification and multi-source occurrence validation

## Quick Example

``` r
library(taxinfo)
library(MiscMetabar)

# Load example fungal data from MiscMetabar
data("data_fungi_mini", package = "MiscMetabar")

# Verify and clean taxonomic names
data_clean <- gna_verifier_pq(data_fungi_mini, 
                             data_sources = 210,
                             add_to_phyloseq = TRUE)

# Add GBIF occurrence data
data_enriched <- tax_gbif_occur_pq(data_clean,
                                  add_to_phyloseq = TRUE)

# View enriched taxonomic table
head(data_enriched@tax_table)
```

## Documentation

Visit the [full documentation website](https://adrientaudiere.github.io/taxinfo/) for detailed guides, function references, and examples.

