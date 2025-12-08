# Verify taxonomic assignment using BLAST against NCBI nucleotide database

The idea is to take the binomial taxonomic name assigned to each ASV/OTU
at the Genus_species level, search for sequences in NCBI nucleotide
database corresponding to this taxon name (with some additional filters
including the marker name), retrieve the sequences in fasta format, and
then perform a BLAST search of retrieved sequences against the ASV/OTU
sequences.

We can therefore test for each ASV/OTU if the best BLAST hit corresponds
to the same taxon name as the one assigned to the ASV/OTU. Moreover, we
can also detect some cases where a better taxonomic assignment can be
proposed based on the BLAST results limited to species name already
present in the phyloseq object.

Note that this function need a physeq object and cannot works with a
list of taxonomic names (taxnames is not a parameter of the function).

## Usage

``` r
tax_retroblast_pq(
  physeq,
  taxonomic_rank = "currentCanonicalSimple",
  marker = NULL,
  id_cut = 99,
  retmax = 500,
  add_to_phyloseq = TRUE,
  verbose = TRUE,
  start_date = NULL,
  end_date = NULL,
  min_length = 300,
  max_length = 4000,
  refseq_only = FALSE,
  sup_params = "NOT uncultured[Title] NOT clone[Title]",
  ...
)
```

## Arguments

- physeq:

  (required) A phyloseq object

- taxonomic_rank:

  (required, default = "currentCanonicalSimple") The column(s) present
  in the @tax_table slot of the phyloseq object. Can be a vector of two
  columns (e.g. c("Genus", "Species")).

- marker:

  (required) A character vector of marker names to be used in the search
  term. For example, c("ITS", "internal transcribed spacer") for fungal
  ITS sequences. Note that the marker names should be present in the
  title of the sequences in NCBI nucleotide database.

- id_cut:

  (default: 99) minimum as a good match. A 100 value means that only
  perfect matches are considered as good matches.

- retmax:

  (default: 500) maximum number of sequences to retrieve from NCBI
  nucleotide database for each taxon name.

- add_to_phyloseq:

  (logical, default TRUE) If TRUE, a new phyloseq object is returned
  with new columns in the tax_table.

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- start_date:

  The start date for the search. If NULL (default), the search is not
  limited by date. The date must be in the format "YYYY-MM-DD".

- end_date:

  () The end date for the search. If NULL (default), the search is not
  limited by date. If start_date is not NULL and end_date is NULL, the
  end_date is set to today's date. The date must be in the format
  "YYYY-MM-DD".

- min_length:

  (int) Minimum sequence length to consider in the search.

- max_length:

  (int) Maximum sequence length to consider in the search.

- refseq_only:

  (logical, default FALSE) If TRUE, only sequences from the RefSeq
  database are retrieved. RefSeq is a curated non-redundant database of
  sequences from NCBI. If FALSE, all sequences from NCBI nucleotide
  database are retrieved. Note that using refseq_only = TRUE is
  experimental and may lead to no sequence retrieved for some taxon
  names.

- sup_params:

  (char) Additional parameters to be added to the search term. By
  default set to ("NOT uncultured\[Title\] NOT clone\[Title\]") to
  exclude uncultured and clone sequences.

- ...:

  Additional parameters to be passed to
  \[MiscMetabar::blast_to_phyloseq()\] including: \`nproc\`,
  \`e_value_cut\` and \`args_blastn\`

## Value

Either a list (if add_to_phyloseq = FALSE) or a new phyloseq object, if
add_to_phyloseq = TRUE, with new columns based on the \`tib_retroblast\`
tibble describe below:

The list is composed of two elements: 1. \`tib_retroblast\`: A tibble
with one row for each taxa of the phyloseq object: - \`blast_queried\`:
(logical) queried names for sequences - \`blast_result\`: (logical)
Number of queried names with at least one blast result -
\`good_assign\`: (logical) Number of good assignation (best blast hit
with as the one assigned to the ASV/OTU) - \`alt_assign\`: Number of
alternative assignation proposed (best blast hit with the phyloseq
object) - \`taxa_name\`: Taxonomic name used to query NCBI nucleotide
database

2\. \`entrez_search\`: A list of the rentrez::entrez_search results for
each taxon name

## See also

\[MiscMetabar::blast_to_phyloseq()\], \[rentrez::entrez_search()\]

## Author

Adrien Taudiere

## Examples

``` r
data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_source = 210)
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 45
#> • Taxa submitted for verification: 37
#> • Genus-level only taxa: 2
#> • Total matches found: 25
#> • Synonyms: 4 (including 4 at genus level)
#> • Accepted names: 21 (including 15 at genus level)

res_retro <- tax_retroblast_pq(data_fungi_mini_cleanNames,
  marker = c("ITS", "internal transcribed spacer"),
  retmax = 10, id_cut = 99
)
#> ℹ Processing taxon: Stereum ostrea
#> ℹ Search term: `Stereum ostrea[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ℹ Number of results for Stereum ostrea: 69
#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■                              11% | ETA: 48s
#> ℹ Processing taxon: Ossicaulis lachnopus
#> ■■■■                              11% | ETA: 48s

#> ℹ Search term: `Ossicaulis lachnopus[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■                              11% | ETA: 48s

#> ℹ Number of results for Ossicaulis lachnopus: 6
#> ■■■■                              11% | ETA: 48s

#> ℹ Number of FASTA sequences retrieved: 6
#> ■■■■                              11% | ETA: 48s

#> ■■■■■■                            16% | ETA:  1m
#> ℹ Processing taxon: Stereum hirsutum
#> ■■■■■■                            16% | ETA:  1m

#> ℹ Search term: `Stereum hirsutum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■                            16% | ETA:  1m

#> ℹ Number of results for Stereum hirsutum: 229
#> ■■■■■■                            16% | ETA:  1m

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■                            16% | ETA:  1m

#> ■■■■■■■                           21% | ETA:  1m
#> ℹ Processing taxon: Basidiodendron eyrei
#> ■■■■■■■                           21% | ETA:  1m

#> ℹ Search term: `Basidiodendron eyrei[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■                           21% | ETA:  1m

#> ℹ Number of results for Basidiodendron eyrei: 15
#> ■■■■■■■                           21% | ETA:  1m

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■                           21% | ETA:  1m

#> ■■■■■■■■■                         26% | ETA:  1m
#> ℹ Processing taxon: Sistotrema oblongisporum
#> ■■■■■■■■■                         26% | ETA:  1m

#> ℹ Search term: `Sistotrema oblongisporum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■                         26% | ETA:  1m

#> ℹ Number of results for Sistotrema oblongisporum: 13
#> ■■■■■■■■■                         26% | ETA:  1m

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■                         26% | ETA:  1m

#> No blast query match the score filters
#> ■■■■■■■■■■                        32% | ETA:  1m
#> ℹ Processing taxon: Fomes fomentarius
#> ■■■■■■■■■■                        32% | ETA:  1m

#> ℹ Search term: `Fomes fomentarius[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■                        32% | ETA:  1m

#> ℹ Number of results for Fomes fomentarius: 453
#> ■■■■■■■■■■                        32% | ETA:  1m

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■                        32% | ETA:  1m

#> No blast query match the score filters
#> ■■■■■■■■■■■■                      37% | ETA: 47s
#> ℹ Processing taxon: Mycena renatii
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ℹ Search term: `Mycena renatii[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ℹ Number of results for Mycena renatii: 0
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ℹ Number of FASTA sequences retrieved: 0
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ! No sequence found for Mycena renatii
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ℹ Processing taxon: Cerocorticium molare
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ℹ Search term: `Cerocorticium molare[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ℹ Number of results for Cerocorticium molare: 0
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ℹ Number of FASTA sequences retrieved: 0
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ! No sequence found for Cerocorticium molare
#> ■■■■■■■■■■■■                      37% | ETA: 47s

#> ■■■■■■■■■■■■■■■                   47% | ETA: 33s
#> ℹ Processing taxon: Aporpium canescens
#> ■■■■■■■■■■■■■■■                   47% | ETA: 33s

#> ℹ Search term: `Aporpium canescens[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■                   47% | ETA: 33s

#> ℹ Number of results for Aporpium canescens: 11
#> ■■■■■■■■■■■■■■■                   47% | ETA: 33s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■                   47% | ETA: 33s

#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s
#> ℹ Processing taxon: Hypochnicium analogum
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ℹ Search term: `Hypochnicium analogum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ℹ Number of results for Hypochnicium analogum: 0
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ℹ Number of FASTA sequences retrieved: 0
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ! No sequence found for Hypochnicium analogum
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ℹ Processing taxon: Hyphoderma roseocremeum
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ℹ Search term: `Hyphoderma roseocremeum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ℹ Number of results for Hyphoderma roseocremeum: 10
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 30s

#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 22s
#> ℹ Processing taxon: Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 22s

#> ℹ Search term: `Hyphoderma setigerum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 22s

#> ℹ Number of results for Hyphoderma setigerum: 62
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 22s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 22s

#> No blast query match the score filters
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 20s
#> ℹ Processing taxon: Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 20s

#> ℹ Search term: `Trametes versicolor[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 20s

#> ℹ Number of results for Trametes versicolor: 959
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 20s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 20s

#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 17s
#> ℹ Processing taxon: Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 17s

#> ℹ Search term: `Peniophora versiformis[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 17s

#> ℹ Number of results for Peniophora versiformis: 12
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 17s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 17s

#> No blast query match the score filters
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s
#> ℹ Processing taxon: Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ℹ Search term: `Exidia glandulosa[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ℹ Number of results for Exidia glandulosa: 62
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s
#> ℹ Processing taxon: Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> ℹ Search term: `Peniophorella pubera[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> ℹ Number of results for Peniophorella pubera: 53
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  7s
#> ℹ Processing taxon: Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  7s

#> ℹ Search term: `Auricularia mesenterica[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  7s

#> ℹ Number of results for Auricularia mesenterica: 31
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  7s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  7s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s
#> ℹ Processing taxon: Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> ℹ Search term: `Hericium coralloides[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> ℹ Number of results for Hericium coralloides: 82
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Processing taxon: Xylodon flaviporus
#> ℹ Search term: `Xylodon flaviporus[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ℹ Number of results for Xylodon flaviporus: 146
#> ℹ Number of FASTA sequences retrieved: 10

res_retro$tib_retroblast |>
  summarise(
    prop_good_assign = sum(good_assign) / sum(blast_result),
    n_alt_assign = sum(!is.na(alt_assign))
  )
#> Error in res_retro$tib_retroblast: $ operator not defined for this S4 class

table(res_retro$tib_retroblast$alt_assign)
#> Error in res_retro$tib_retroblast: $ operator not defined for this S4 class

res_retro_100 <- tax_retroblast_pq(data_fungi_mini_cleanNames,
  marker = c("ITS", "internal transcribed spacer"),
  retmax = 100, id_cut = 100
)
#> ■■■                                5% | ETA:  0s
#> ℹ Processing taxon: Stereum ostrea
#> ■■■                                5% | ETA:  0s

#> ℹ Search term: `Stereum ostrea[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■                                5% | ETA:  0s

#> ℹ Number of results for Stereum ostrea: 69
#> ■■■                                5% | ETA:  0s

#> ℹ Number of FASTA sequences retrieved: 69
#> ■■■                                5% | ETA:  0s

#> ■■■■                              11% | ETA: 42s
#> ℹ Processing taxon: Ossicaulis lachnopus
#> ■■■■                              11% | ETA: 42s

#> ℹ Search term: `Ossicaulis lachnopus[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■                              11% | ETA: 42s

#> ℹ Number of results for Ossicaulis lachnopus: 6
#> ■■■■                              11% | ETA: 42s

#> ℹ Number of FASTA sequences retrieved: 6
#> ■■■■                              11% | ETA: 42s

#> ■■■■■■                            16% | ETA: 47s
#> ℹ Processing taxon: Stereum hirsutum
#> ■■■■■■                            16% | ETA: 47s

#> ℹ Search term: `Stereum hirsutum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■                            16% | ETA: 47s

#> ℹ Number of results for Stereum hirsutum: 229
#> ■■■■■■                            16% | ETA: 47s

#> ℹ Number of FASTA sequences retrieved: 100
#> ■■■■■■                            16% | ETA: 47s

#> ■■■■■■■                           21% | ETA:  1m
#> ℹ Processing taxon: Basidiodendron eyrei
#> ■■■■■■■                           21% | ETA:  1m

#> ℹ Search term: `Basidiodendron eyrei[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■                           21% | ETA:  1m

#> ℹ Number of results for Basidiodendron eyrei: 15
#> ■■■■■■■                           21% | ETA:  1m

#> ℹ Number of FASTA sequences retrieved: 15
#> ■■■■■■■                           21% | ETA:  1m

#> ■■■■■■■■■                         26% | ETA:  1m
#> ℹ Processing taxon: Sistotrema oblongisporum
#> ■■■■■■■■■                         26% | ETA:  1m

#> ℹ Search term: `Sistotrema oblongisporum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■                         26% | ETA:  1m

#> ℹ Number of results for Sistotrema oblongisporum: 13
#> ■■■■■■■■■                         26% | ETA:  1m

#> ℹ Number of FASTA sequences retrieved: 13
#> ■■■■■■■■■                         26% | ETA:  1m

#> No blast query match the score filters
#> ■■■■■■■■■■                        32% | ETA:  1m
#> ℹ Processing taxon: Fomes fomentarius
#> ■■■■■■■■■■                        32% | ETA:  1m

#> ℹ Search term: `Fomes fomentarius[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■                        32% | ETA:  1m

#> ℹ Number of results for Fomes fomentarius: 453
#> ■■■■■■■■■■                        32% | ETA:  1m

#> ℹ Number of FASTA sequences retrieved: 100
#> ■■■■■■■■■■                        32% | ETA:  1m

#> ■■■■■■■■■■■■                      37% | ETA: 49s
#> ℹ Processing taxon: Mycena renatii
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Search term: `Mycena renatii[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Number of results for Mycena renatii: 0
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Number of FASTA sequences retrieved: 0
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ! No sequence found for Mycena renatii
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Processing taxon: Cerocorticium molare
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Search term: `Cerocorticium molare[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Number of results for Cerocorticium molare: 0
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Number of FASTA sequences retrieved: 0
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ! No sequence found for Cerocorticium molare
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Processing taxon: Aporpium canescens
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Search term: `Aporpium canescens[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Number of results for Aporpium canescens: 11
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ℹ Number of FASTA sequences retrieved: 11
#> ■■■■■■■■■■■■                      37% | ETA: 49s

#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 31s
#> ℹ Processing taxon: Hypochnicium analogum
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 31s

#> ℹ Search term: `Hypochnicium analogum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 31s

#> ℹ Number of results for Hypochnicium analogum: 0
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 31s

#> ℹ Number of FASTA sequences retrieved: 0
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 31s

#> ! No sequence found for Hypochnicium analogum
#> ■■■■■■■■■■■■■■■■■                 53% | ETA: 31s

#> ■■■■■■■■■■■■■■■■■■                58% | ETA: 26s
#> ℹ Processing taxon: Hyphoderma roseocremeum
#> ■■■■■■■■■■■■■■■■■■                58% | ETA: 26s

#> ℹ Search term: `Hyphoderma roseocremeum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■                58% | ETA: 26s

#> ℹ Number of results for Hyphoderma roseocremeum: 10
#> ■■■■■■■■■■■■■■■■■■                58% | ETA: 26s

#> ℹ Number of FASTA sequences retrieved: 10
#> ■■■■■■■■■■■■■■■■■■                58% | ETA: 26s

#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 23s
#> ℹ Processing taxon: Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 23s

#> ℹ Search term: `Hyphoderma setigerum[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 23s

#> ℹ Number of results for Hyphoderma setigerum: 62
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 23s

#> ℹ Number of FASTA sequences retrieved: 62
#> ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 23s

#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 21s
#> ℹ Processing taxon: Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 21s

#> ℹ Search term: `Trametes versicolor[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 21s

#> ℹ Number of results for Trametes versicolor: 959
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 21s

#> ℹ Number of FASTA sequences retrieved: 100
#> ■■■■■■■■■■■■■■■■■■■■■■            68% | ETA: 21s

#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 18s
#> ℹ Processing taxon: Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 18s

#> ℹ Search term: `Peniophora versiformis[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 18s

#> ℹ Number of results for Peniophora versiformis: 12
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 18s

#> ℹ Number of FASTA sequences retrieved: 12
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA: 18s

#> No blast query match the score filters
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s
#> ℹ Processing taxon: Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ℹ Search term: `Exidia glandulosa[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ℹ Number of results for Exidia glandulosa: 62
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ℹ Number of FASTA sequences retrieved: 62
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA: 14s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s
#> ℹ Processing taxon: Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> ℹ Search term: `Peniophorella pubera[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> ℹ Number of results for Peniophorella pubera: 53
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> ℹ Number of FASTA sequences retrieved: 53
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA: 11s

#> No blast query match the score filters
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  8s
#> ℹ Processing taxon: Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  8s

#> ℹ Search term: `Auricularia mesenterica[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  8s

#> ℹ Number of results for Auricularia mesenterica: 31
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  8s

#> ℹ Number of FASTA sequences retrieved: 31
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  8s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s
#> ℹ Processing taxon: Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> ℹ Search term: `Hericium coralloides[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> ℹ Number of results for Hericium coralloides: 82
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> ℹ Number of FASTA sequences retrieved: 82
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  4s

#> No blast query match the score filters
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ Processing taxon: Xylodon flaviporus
#> ℹ Search term: `Xylodon flaviporus[Organism] AND (ITS[Title] OR internal transcribed spacer[Title]) AND 300:4000[SLEN] NOT uncultured[Title] NOT clone[Title]`
#> ℹ Number of results for Xylodon flaviporus: 146
#> ℹ Number of FASTA sequences retrieved: 100

# nb of queried names for sequences (id=100%)
res_retro_100$tib_retroblast$blast_queried |> sum()
#> Error in res_retro_100$tib_retroblast: $ operator not defined for this S4 class
# nb of queried names with at least one blast result (id=100%)
res_retro_100$tib_retroblast$blast_result |> sum()
#> Error in res_retro_100$tib_retroblast: $ operator not defined for this S4 class
# nb of good assignation (id=100%)
res_retro_100$tib_retroblast$good_assign |> sum()
#> Error in res_retro_100$tib_retroblast: $ operator not defined for this S4 class
# nb of alternative assignation proposed (id=100%)
res_retro_100$tib_retroblast$alt_assign |>
  is.na() |>
  sapply(isFALSE) |>
  sum()
#> Error in res_retro_100$tib_retroblast: $ operator not defined for this S4 class
```
