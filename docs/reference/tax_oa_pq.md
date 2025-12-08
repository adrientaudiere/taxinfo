# Get scientific works about taxa present in a phyloseq object

A wrapper of \[openalexR::oa_fetch()\] function to get the number of
scientific works (and a list of doi if count_only is set to FALSE) for
each taxa of a phyloseq object

## Usage

``` r
tax_oa_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  count_only = FALSE,
  return_raw_oa = FALSE,
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  type_works = c("article", "review", "book-chapter", "book", "letter"),
  verbose = TRUE,
  ...
)
```

## Arguments

- physeq:

  (optional) A phyloseq object. Either \`physeq\` or \`taxnames\` must
  be provided, but not both.

- taxnames:

  (optional) A character vector of taxonomic names.

- taxonomic_rank:

  (Character, default "currentCanonicalSimple") The column(s) present in
  the @tax_table slot of the phyloseq object. Can be a vector of two
  columns (e.g. c("Genus", "Species")).

- count_only:

  (Logical, default FALSE) If TRUE, only the number of works on a given
  taxa is return, leading to a faster call to \`openalexR::oa_fetch()\`.
  Note that if count_only is set to TRUE all works (including e.g.
  preprint and dataset) are count, leading to higher number of works
  than if count_only is set to FALSE (see parameter \`type_works\`).

- return_raw_oa:

  (Logical, default FALSE) If TRUE, return the raw list of publications
  from Open Alex for each taxa as a list of data.frame. Can be useful to
  filter works for example by topic or by number of citations (see
  section examples).

- add_to_phyloseq:

  (logical, default TRUE when physeq is provided, FALSE when taxnames is
  provided) If TRUE, return a new phyloseq object with new columns in
  the tax_table slot. Automatically set to TRUE when a phyloseq object
  is provided and FALSE when taxnames is provided. Cannot be TRUE if
  \`taxnames\` is provided.

- col_prefix:

  A character string to be added as a prefix to the new columns names
  added to the tax_table slot of the phyloseq object (default: NULL).

- type_works:

  (A list of type to select) See Open Alex
  \[documentation\](https://docs.openalex.org/api-entities/works/work-object#type).
  Only used if count_only is set to FALSE Default is c("article",
  "review", "book-chapter", "book", "letter").

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- ...:

  Other params to passed on \[openalexR::oa_fetch()\]

## Value

Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq object,
if add_to_phyloseq = TRUE, with 1 (\`n_doi\`) or 4 (\`n_doi\`,
\`list_doi\`, \`n_citation\` and \`list_keywords\` if \`count_only\` is
FALSE) new column(s) in the tax_table.

## Details

This function is mainly a wrapper of the work of others. Please cite
\`openalexR\` package.

## Author

Adrien Taudiere

## Examples

``` r
data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini) |>
  tax_oa_pq()
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 45
#> • Taxa submitted for verification: 37
#> • Genus-level only taxa: 2
#> • Total matches found: 25
#> • Synonyms: 2 (including 2 at genus level)
#> • Accepted names: 23 (including 21 at genus level)
#> Fetching OpenAlex ■                                  0% |  ETA: ?
#> ℹ Fetching OpenAlex works for taxon: Stereum ostrea
#> Fetching OpenAlex ■                                  0% |  ETA: ?

#> Fetching OpenAlex ■■                                 4% |  ETA:  1m
#> ℹ Fetching OpenAlex works for taxon: Xylodon raduloides
#> Fetching OpenAlex ■■                                 4% |  ETA:  1m

#> ℹ Fetching OpenAlex works for taxon: Stereum hirsutum
#> Fetching OpenAlex ■■                                 4% |  ETA:  1m

#> Fetching OpenAlex ■■■■■                             13% |  ETA:  1m
#> ℹ Fetching OpenAlex works for taxon: Trametopsis brasiliensis
#> Fetching OpenAlex ■■■■■                             13% |  ETA:  1m

#> ℹ Fetching OpenAlex works for taxon: Basidiodendron eyrei
#> Fetching OpenAlex ■■■■■                             13% |  ETA:  1m

#> ℹ Fetching OpenAlex works for taxon: Sistotrema oblongisporum
#> Fetching OpenAlex ■■■■■                             13% |  ETA:  1m

#> Fetching OpenAlex ■■■■■■■■■                         26% |  ETA: 33s
#> ℹ Fetching OpenAlex works for taxon: Fomes fomentarius
#> Fetching OpenAlex ■■■■■■■■■                         26% |  ETA: 33s

#> Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA:  1m
#> ℹ Fetching OpenAlex works for taxon: Mycena renati
#> Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA:  1m

#> Fetching OpenAlex ■■■■■■■■■■■                       35% |  ETA: 43s
#> ℹ Fetching OpenAlex works for taxon: Helicogloea pellucida
#> Fetching OpenAlex ■■■■■■■■■■■                       35% |  ETA: 43s

#> ℹ Fetching OpenAlex works for taxon: Radulomyces molaris
#> Fetching OpenAlex ■■■■■■■■■■■                       35% |  ETA: 43s

#> ℹ Fetching OpenAlex works for taxon: Elmerina caryae
#> Fetching OpenAlex ■■■■■■■■■■■                       35% |  ETA: 43s

#> ℹ Fetching OpenAlex works for taxon: Phanerochaete livescens
#> Fetching OpenAlex ■■■■■■■■■■■                       35% |  ETA: 43s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■                 52% |  ETA: 24s
#> ℹ Fetching OpenAlex works for taxon: Gloeohypochnicium analogum
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■                 52% |  ETA: 24s

#> ℹ Fetching OpenAlex works for taxon: Hyphoderma roseocremeum
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■                 52% |  ETA: 24s

#> ℹ Fetching OpenAlex works for taxon: Hyphoderma setigerum
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■                 52% |  ETA: 24s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■             65% |  ETA: 16s
#> ℹ Fetching OpenAlex works for taxon: Trametes versicolor
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■             65% |  ETA: 16s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■            70% |  ETA: 46s
#> ℹ Fetching OpenAlex works for taxon: Peniophora versiformis
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■            70% |  ETA: 46s

#> ℹ Fetching OpenAlex works for taxon: Exidia glandulosa
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■            70% |  ETA: 46s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA: 30s
#> ℹ Fetching OpenAlex works for taxon: Peniophorella pubera
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA: 30s

#> ℹ Fetching OpenAlex works for taxon: Auricularia mesenterica
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA: 30s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% |  ETA: 17s
#> ℹ Fetching OpenAlex works for taxon: Laetisaria buckii
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% |  ETA: 17s

#> Warning: No records found!
#> ℹ Fetching OpenAlex works for taxon: Hericium coralloides
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% |  ETA: 17s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    96% |  ETA:  5s
#> ℹ Fetching OpenAlex works for taxon: Xylodon flaviporus
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    96% |  ETA:  5s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% |  ETA:  0s
#> 

ggplot(
  subset_taxa(data_fungi_mini_cleanNames, !is.na(n_doi))@tax_table,
  aes(
    x = log10(as.numeric(n_doi)),
    y = forcats::fct_reorder(currentCanonicalSimple, as.numeric(n_doi))
  )
) +
  geom_point(aes(col = Order)) +
  xlab("Number of Scientific Papers (log10 scale)")


tax_oa_pq(data_fungi_mini_cleanNames, type_works = "dataset")
#> ℹ Fetching OpenAlex works for taxon: Stereum ostrea
#> Fetching OpenAlex ■■                                 4% |  ETA: 49s
#> ℹ Fetching OpenAlex works for taxon: Xylodon raduloides
#> Fetching OpenAlex ■■                                 4% |  ETA: 49s

#> ℹ Fetching OpenAlex works for taxon: Stereum hirsutum
#> Fetching OpenAlex ■■                                 4% |  ETA: 49s

#> Fetching OpenAlex ■■■■■                             13% |  ETA:  1m
#> ℹ Fetching OpenAlex works for taxon: Trametopsis brasiliensis
#> Fetching OpenAlex ■■■■■                             13% |  ETA:  1m

#> ℹ Fetching OpenAlex works for taxon: Basidiodendron eyrei
#> Fetching OpenAlex ■■■■■                             13% |  ETA:  1m

#> Fetching OpenAlex ■■■■■■■■                          22% |  ETA: 39s
#> ℹ Fetching OpenAlex works for taxon: Sistotrema oblongisporum
#> Fetching OpenAlex ■■■■■■■■                          22% |  ETA: 39s

#> ℹ Fetching OpenAlex works for taxon: Fomes fomentarius
#> Fetching OpenAlex ■■■■■■■■                          22% |  ETA: 39s

#> Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA:  1m
#> ℹ Fetching OpenAlex works for taxon: Mycena renati
#> Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA:  1m

#> ℹ Fetching OpenAlex works for taxon: Helicogloea pellucida
#> Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA:  1m

#> ℹ Fetching OpenAlex works for taxon: Radulomyces molaris
#> Fetching OpenAlex ■■■■■■■■■■                        30% |  ETA:  1m

#> Fetching OpenAlex ■■■■■■■■■■■■■■                    43% |  ETA: 33s
#> ℹ Fetching OpenAlex works for taxon: Elmerina caryae
#> Fetching OpenAlex ■■■■■■■■■■■■■■                    43% |  ETA: 33s

#> ℹ Fetching OpenAlex works for taxon: Phanerochaete livescens
#> Fetching OpenAlex ■■■■■■■■■■■■■■                    43% |  ETA: 33s

#> ℹ Fetching OpenAlex works for taxon: Gloeohypochnicium analogum
#> Fetching OpenAlex ■■■■■■■■■■■■■■                    43% |  ETA: 33s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■                57% |  ETA: 22s
#> ℹ Fetching OpenAlex works for taxon: Hyphoderma roseocremeum
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■                57% |  ETA: 22s

#> ℹ Fetching OpenAlex works for taxon: Hyphoderma setigerum
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■                57% |  ETA: 22s

#> ℹ Fetching OpenAlex works for taxon: Trametes versicolor
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■                57% |  ETA: 22s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■            70% |  ETA: 47s
#> ℹ Fetching OpenAlex works for taxon: Peniophora versiformis
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■            70% |  ETA: 47s

#> ℹ Fetching OpenAlex works for taxon: Exidia glandulosa
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■            70% |  ETA: 47s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA: 31s
#> ℹ Fetching OpenAlex works for taxon: Peniophorella pubera
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA: 31s

#> ℹ Fetching OpenAlex works for taxon: Auricularia mesenterica
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA: 31s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% |  ETA: 17s
#> ℹ Fetching OpenAlex works for taxon: Laetisaria buckii
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% |  ETA: 17s

#> Warning: No records found!
#> ℹ Fetching OpenAlex works for taxon: Hericium coralloides
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% |  ETA: 17s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    96% |  ETA:  5s
#> ℹ Fetching OpenAlex works for taxon: Xylodon flaviporus
#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    96% |  ETA:  5s

#> Fetching OpenAlex ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% |  ETA:  0s
#> 
#> Warning: Column names already exist in tax_table: "n_doi", "list_doi", "n_citation", and
#> "list_keywords"
#> ℹ Adding prefix 'oa_' to avoid conflicts
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 45 taxa and 137 samples ]
#> sample_data() Sample Data:       [ 137 samples by 7 sample variables ]
#> tax_table()   Taxonomy Table:    [ 45 taxa by 28 taxonomic ranks ]
#> refseq()      DNAStringSet:      [ 45 reference sequences ]


list_pub_raw <- tax_oa_pq(data_fungi_mini_cleanNames,
  return_raw_oa = TRUE
)
#> Error in tax_oa_pq(data_fungi_mini_cleanNames, return_raw_oa = TRUE): You can not set to TRUE more than one of the parameters return_raw_oa and add_to_phyloseq.

list_pub_Health_science <- lapply(list_pub_raw, function(xx) {
  if (length(xx) == 0) {
    return(NULL)
  } else {
    filter(xx, map_lgl(topics, function(tibble_item) {
      if (is.null(tibble_item) || nrow(tibble_item) == 0) {
        return(FALSE)
      } else {
        any(grepl("Health science",
          tibble_item$display_name[tibble_item$type == "domain"],
          ignore.case = TRUE
        ))
      }
    }))
  }
})
#> Error: object 'list_pub_raw' not found


list_pub_Ecology <- lapply(list_pub_raw, function(xx) {
  if (length(xx) == 0) {
    return(NULL)
  } else {
    filter(xx, map_lgl(topics, function(tibble_item) {
      if (is.null(tibble_item) || nrow(tibble_item) == 0) {
        return(FALSE)
      } else {
        any(grepl("Ecology",
          tibble_item$display_name[tibble_item$type == "subfield"],
          ignore.case = TRUE
        ))
      }
    }))
  }
})
#> Error: object 'list_pub_raw' not found

list_pub_at_least_ten_citations <-
  lapply(list_pub_raw, function(xx) {
    if (length(xx) == 0) {
      return(NULL)
    } else {
      filter(xx, cited_by_count > 10)
    }
  })
#> Error: object 'list_pub_raw' not found
```
