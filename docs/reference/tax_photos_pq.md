# Find photos of taxa from GBIF or Wikitaxa

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

## Usage

``` r
tax_photos_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  source = "gbif",
  folder_name = "photos_physeq",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  gallery = FALSE,
  overwrite_folder = FALSE,
  col_name_url = "photo_url",
  verbose = TRUE,
  caption_valign = "bottom",
  caption_font_size = 12,
  simple_caption = FALSE,
  img_height = "150px",
  img_width = "200px",
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE,
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

  (Character, default = "currentCanonicalSimple") The column(s) present
  in the @tax_table slot of the phyloseq object. Can be a vector of two
  columns (e.g. the c("Genus", "Species")).

- source:

  (Character) either "gbif" or "wikitaxa".

- folder_name:

  (default "photos_physeq") Name of the folder where photos will be
  downloaded. Only used if both add_to_phyloseq and gallery are FALSE.

- add_to_phyloseq:

  (logical, default TRUE when physeq is provided, FALSE when taxnames is
  provided) If TRUE, a new phyloseq object is returned with a new column
  containing the URL (entitled with the parameter col_name_url) in the
  tax_table. Automatically set to TRUE when a phyloseq object is
  provided and FALSE when taxnames is provided. Cannot be TRUE if
  \`taxnames\` is provided.

- col_prefix:

  A character string to be added as a prefix to the new columns names
  added to the tax_table slot of the phyloseq object (default: NULL).

- gallery:

  (logical, default FALSE) If TRUE, a html gallery is created using
  \[htmltools::browsable()\].

- overwrite_folder:

  (logical, default FALSE) If TRUE, the folder specified in the
  parameter folder_name will be deleted if it already exists.

- col_name_url:

  (default "photo_url") Name of the new column in the tax_table

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- caption_valign:

  (character, default "bottom") Vertical alignment of the caption in the
  gallery. Either \`"bottom"\` or \`"top"\`.

- caption_font_size:

  (int) Size of the caption font in the gallery.

- simple_caption:

  (logical, default FALSE) If TRUE, the caption of the gallery photo
  will be only the taxonomic name. If FALSE, the caption include
  information from the phyloseq object (number of sequences, taxa and
  samples).

- img_height:

  (character, default "150px") Height of images in the gallery.

- img_width:

  (character, default "200px") Width of images in the gallery.

- discard_genus_alone:

  (logical, default \`TRUE\` when \`taxonomic_rank ==
  "currentCanonicalSimple"\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- discard_NA:

  (logical, default \`TRUE\`). Passed to
  \[taxonomic_rank_to_taxnames()\].

- ...:

  Unused, kept for backward compatibility.

## Value

There is three behavior.(i) If add_to_phyloseq = TRUE, a new phyloseq
object is returned with a new column (called with the parameter
col_name_url) in the tax_table containing the URL; the gallery is
printed as a side-effect if \`gallery = TRUE\`. (ii) If add_to_phyloseq
= FALSE and gallery = TRUE, the HTML gallery is returned. (iii) If both
gallery = FALSE and add_to_phyloseq = FALSE, photos are downloaded in a
folder (folder_name parameter) and the list of url are returned in the
form of a tibble.

## Details

There is three behavior. See the returns section. Gbif source is quicker
than wikitaxa source. Note that for the moment the function only return
one photo per species.

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)

tax_photos_pq(data_fungi_mini_cleanNames,
  gallery = TRUE,
  img_height = "40px",
  img_width = "80px",
  source = "wikitaxa"
)

tax_photos_pq(
  taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
  gallery = TRUE
)

data_fungi_mini_cleanNames_photos <-
  tax_photos_pq(data_fungi_mini_cleanNames)

# Which photo(s) depicted more than one OTU
data_fungi_mini_cleanNames_photos@tax_table[, "photo_url"] |>
  table() |>
  (\(tab) tab[as.numeric(tab) > 1])()
} # }
```
