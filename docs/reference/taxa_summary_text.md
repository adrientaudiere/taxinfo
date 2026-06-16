# Text summary for a taxonomic rank

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Create a text to summarize the number of samples, taxa, sequences and
occurrences of selected taxa in a phyloseq object for a given value in
the column of a tax_table

## Usage

``` r
taxa_summary_text(
  physeq,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  verbose = TRUE,
  min_nb_seq = 0,
  ...
)
```

## Arguments

- physeq:

  A phyloseq object

- taxnames:

  (optional) A character vector of taxonomic names.

- taxonomic_rank:

  (Character, default "currentCanonicalSimple") The column(s) present in
  the @tax_table slot of the phyloseq object. Can be a vector of two
  columns (e.g. c("Genus", "Species")).

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- min_nb_seq:

  minimum number of sequences by OTUs by samples to take into count this
  OTUs in this sample. For example, if min_nb_seq=2,each value of 2 or
  less in the OTU table will not count in the venn diagram

- ...:

  Additional arguments to pass to \[subset_taxa_pq()\].

## Value

A character string summarizing the number of samples, taxa, sequences
and occurrences of the selected taxa.

## Author

Adrien Taudiere

## Examples

``` r
data_fungi_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 45
#> • Taxa submitted for verification: 37
#> • Genus-level only taxa: 2
#> • Total matches found: 25
#> • Synonyms: 4 (including 4 at genus level)
#> • Accepted names: 21 (including 15 at genus level)

taxa_summary_text(data_fungi_cleanNames, taxnames = "Xylodon flaviporus")
#> Warning: At least one of your sample contains less than 500 sequences.
#> Warning: At least one of your taxa is represent by less than 1 sequences.
#> Warning: At least one of your samples metadata columns contains NA.
#> Number of non-matching ASV 0
#> Number of matching ASV 45
#> Number of filtered-out ASV 44
#> Number of kept ASV 1
#> Number of kept samples 137
#> [1] "Xylodon flaviporus: 4 samp., 1 taxa, 5467 seq., 4 occ."
# \donttest{
taxa_summary_text(data_fungi_cleanNames,
  taxnames = "Xylodon flaviporus",
  min_nb_seq = 100, verbose = FALSE
)
#> [1] "Xylodon flaviporus: 1 samp., 1 taxa, 5397 seq., 1 occ."
taxa_summary_text(data_fungi_cleanNames,
  taxonomic_rank = "Trait",
  taxnames = c("Soft Rot"), verbose = FALSE
)
#> Error in select_taxa_pq(physeq = physeq, taxonomic_rank = taxonomic_rank,     taxnames = taxnames, verbose = verbose, clean_pq = FALSE,     ...): No taxa match the requested `taxnames`.
#> ℹ Searched the "Trait" tax_table column(s) for "Soft Rot".
# }
```
