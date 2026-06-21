---
title: 'Taxinfo: Augment 'Phyloseq' Objects with Taxonomy-Based Information'
tags:
  - R
  - Bioinformatic
  - Metagenomics
  - Barcoding
  - Reproducibility
authors:
  - name: Adrien Taudière
    orcid: 0000-0003-1088-1182
    affiliation: "1"
  - name: Mélanie Roy
    orcid: 0000-0002-4565-2331
    affiliation: "2"
  - name: Benoît Perez-Lamarque
    orcid: 0000-0001-7112-7197
    affiliation: "3"
affiliations:
 - name: IdEst, Saint-Bonnet-de-Salendrinque, 30460 France
   index: 1
 - name: Université Toulouse III - Paul Sabatier: Toulouse, FR 
   index: 2
 - name: Université Toulouse III - Paul Sabatier: Toulouse, FR 
   index: 3

date:  XXX
bibliography: paper.bib
---

# Summary

# Statement of Need

Metabarcoding is now a widely used method for studying biodiversity, conservation and ecosystem functioning (REF). By sequencing little region of the DNA of organisms, metabarcoding can be used to caracterize communities of organisms, sometimes at the species level (REF). A popular structure to analyze the data arising from metabarcoding is the phyloseq class of the phyloseq package (REF). When some taxonomic information is available, phyloseq objects can be enriched with taxonomic-based information from various external data sources. This paper presents the **taxinfo** package, a set of functions to augment phyloseq objects with taxonomic-based information from various external data sources such as GBIF, Wikipedia, GLOBI, OpenAlex, and other databases.


# State of the Field in R

# Features

## Clean and harmonize taxonomic names

The first step to enable the communication between database and phyloseq objects is to clean and harmonize taxonomic names. This is achieved by the `gna_verifier_pq()` function. It verifies and standardizes taxonomic names using the Global Names Architecture (GNA) API (REF) and adds the results to the phyloseq object.

## Augment phyloseq objects with taxonomic-based information from external data sources


## Consolidate metabarcoding data and point out surprising taxons




# Acknowledgements


# References
