# Find photos of taxa from GBIF or Wikitaxa

Find photos of taxa from GBIF or Wikitaxa

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

  (logical, default FALSE) If TRUE, a html gallery is created using the
  function \[pixture::pixgallery()\].

- overwrite_folder:

  (logical, default FALSE) If TRUE, the folder specified in the
  parameter folder_name will be deleted if it already exists.

- col_name_url:

  (default "photo_url") Name of the new column in the tax_table

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

- caption_valign:

  (character, default "bottom") Vertical alignment of the caption in the
  gallery.

- caption_font_size:

  (int) Size of the caption font in the gallery.

- simple_caption:

  (logical, default FALSE) If TRUE, the caption of the gallery photo
  will be only the taxonomic name. If FALSE, the caption include
  information from the phyloseq object (number of sequences, taxa and
  samples).

- ...:

  Other parameters to be passed to pixture::pixgallery() function.

## Value

There is three behavior.(i) If gallery = TRUE, a html gallery is created
using the function \[pixture::pixgallery()\]. (ii) If add_to_phyloseq =
TRUE, a new phyloseq object is returned with a new column (called with
the parameter col_name_url) in the tax_table containing the URL. (iii)
If both gallery = FALSE and add_to_phyloseq = FALSE, photos are
downloaded in a folder (folder_name parameter) and the list of url are
returned in the form of a tibble.

## Details

There is three behavior. See the returns section. Gbif source is quicker
than wikitaxa source. Note that for the moment the function only return
one photo per species.

## Author

Adrien Taudiere

## Examples

``` r
data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
#> ✔ GNA verification summary:
#> • Total taxa in phyloseq: 45
#> • Taxa submitted for verification: 37
#> • Genus-level only taxa: 2
#> • Total matches found: 25
#> • Synonyms: 2 (including 2 at genus level)
#> • Accepted names: 23 (including 21 at genus level)

tax_photos_pq(data_fungi_mini_cleanNames,
  gallery = TRUE,
  h = "40px",
  w = "80px",
  source = "wikitaxa"
)
#> ■■                                 4% | ETA:  0s
#> ℹ 1/23 - Downloading photo of Stereum ostrea
#> ■■                                 4% | ETA:  0s

#> ℹ 2/23 - No photo available for Xylodon raduloides
#> ■■                                 4% | ETA:  0s

#> ■■■■■                             13% | ETA: 21s
#> ℹ 3/23 - Downloading photo of Stereum hirsutum
#> ■■■■■                             13% | ETA: 21s

#> ℹ 4/23 - No photo available for Trametopsis brasiliensis
#> ■■■■■                             13% | ETA: 21s

#> ■■■■■■■■                          22% | ETA: 24s
#> ℹ 5/23 - No photo available for Basidiodendron eyrei
#> ■■■■■■■■                          22% | ETA: 24s

#> ℹ 6/23 - No photo available for Sistotrema oblongisporum
#> ■■■■■■■■                          22% | ETA: 24s

#> ■■■■■■■■■■                        30% | ETA: 20s
#> ℹ 7/23 - Downloading photo of Fomes fomentarius
#> ■■■■■■■■■■                        30% | ETA: 20s

#> ℹ 8/23 - Downloading photo of Mycena renati
#> ■■■■■■■■■■                        30% | ETA: 20s

#> ■■■■■■■■■■■■■                     39% | ETA: 20s
#> ℹ 9/23 - No photo available for Helicogloea pellucida
#> ■■■■■■■■■■■■■                     39% | ETA: 20s

#> ℹ 10/23 - No photo available for Radulomyces molaris
#> ■■■■■■■■■■■■■                     39% | ETA: 20s

#> ■■■■■■■■■■■■■■■                   48% | ETA: 17s
#> ℹ 11/23 - No photo available for Elmerina caryae
#> ■■■■■■■■■■■■■■■                   48% | ETA: 17s

#> ℹ 12/23 - No photo available for Phanerochaete livescens
#> ■■■■■■■■■■■■■■■                   48% | ETA: 17s

#> ℹ 13/23 - No photo available for Gloeohypochnicium analogum
#> ■■■■■■■■■■■■■■■                   48% | ETA: 17s

#> ■■■■■■■■■■■■■■■■■■■               61% | ETA: 12s
#> ℹ 14/23 - No photo available for Hyphoderma roseocremeum
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA: 12s

#> ℹ 15/23 - No photo available for Hyphoderma setigerum
#> ■■■■■■■■■■■■■■■■■■■               61% | ETA: 12s

#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  9s
#> ℹ 16/23 - Downloading photo of Trametes versicolor
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  9s

#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA:  8s
#> ℹ 17/23 - No photo available for Peniophora versiformis
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA:  8s

#> ℹ 18/23 - Downloading photo of Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA:  8s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  6s
#> ℹ 19/23 - No photo available for Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  6s

#> ℹ 20/23 - No photo available for Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  6s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  3s
#> ℹ 21/23 - No photo available for Laetisaria buckii
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  3s

#> ℹ 22/23 - Downloading photo of Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  3s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ 23/23 - No photo available for Xylodon flaviporus
#> ✔ Photo download summary:/n
#> - 7 photos found and downloaded/n
#> - 13 taxa depicted/n
#> - 16 taxonomic names not found/n
#> - 32 taxa have no photo URL
#> ℹ Creating captions for gallery

{"x":{"path":["https://upload.wikimedia.org/wikipedia/commons/b/b3/Stereum_ostrea_51466.jpg","https://upload.wikimedia.org/wikipedia/commons/0/00/Stereum_hirsutum_-_False_Turkey_Tail.jpg","https://upload.wikimedia.org/wikipedia/commons/4/44/Fomes_fomentarius_(46906865784).jpg","https://upload.wikimedia.org/wikipedia/commons/9/98/Mycena_renati_509659080.jpg","https://upload.wikimedia.org/wikipedia/commons/5/5d/Stumpfungus.jpg","https://upload.wikimedia.org/wikipedia/commons/d/d6/Exidia_glandulosa_74739.jpg","https://upload.wikimedia.org/wikipedia/commons/1/12/2009-09-25_Hericium_coralloides_(Scop.)_Pers_58068_crop.jpg"],"caption":["<p style='font-size:12px'> <b>Stereum ostrea<\/b><br> <b>Source<\/b>: <a href='https://upload.wikimedia.org/wikipedia/commons/b/b3/Stereum_ostrea_51466.jpg'>Wikimedia<\/a><br> <b>Taxa<\/b>: 3 , <b>Seq<\/b>: 80067 <b>, Sam<\/b>: 93<\/p>","<p style='font-size:12px'> <b>Stereum hirsutum<\/b><br> <b>Source<\/b>: <a href='https://upload.wikimedia.org/wikipedia/commons/0/00/Stereum_hirsutum_-_False_Turkey_Tail.jpg'>Wikimedia<\/a><br> <b>Taxa<\/b>: 1 , <b>Seq<\/b>: 20660 <b>, Sam<\/b>: 13<\/p>","<p style='font-size:12px'> <b>Fomes fomentarius<\/b><br> <b>Source<\/b>: <a href='https://upload.wikimedia.org/wikipedia/commons/4/44/Fomes_fomentarius_(46906865784).jpg'>Wikimedia<\/a><br> <b>Taxa<\/b>: 4 , <b>Seq<\/b>: 40207 <b>, Sam<\/b>: 9<\/p>","<p style='font-size:12px'> <b>Mycena renati<\/b><br> <b>Source<\/b>: <a href='https://upload.wikimedia.org/wikipedia/commons/9/98/Mycena_renati_509659080.jpg'>Wikimedia<\/a><br> <b>Taxa<\/b>: 1 , <b>Seq<\/b>: 12922 <b>, Sam<\/b>: 10<\/p>","<p style='font-size:12px'> <b>Trametes versicolor<\/b><br> <b>Source<\/b>: <a href='https://upload.wikimedia.org/wikipedia/commons/5/5d/Stumpfungus.jpg'>Wikimedia<\/a><br> <b>Taxa<\/b>: 1 , <b>Seq<\/b>: 8849 <b>, Sam<\/b>: 3<\/p>","<p style='font-size:12px'> <b>Exidia glandulosa<\/b><br> <b>Source<\/b>: <a href='https://upload.wikimedia.org/wikipedia/commons/d/d6/Exidia_glandulosa_74739.jpg'>Wikimedia<\/a><br> <b>Taxa<\/b>: 2 , <b>Seq<\/b>: 12493 <b>, Sam<\/b>: 2<\/p>","<p style='font-size:12px'> <b>Hericium coralloides<\/b><br> <b>Source<\/b>: <a href='https://upload.wikimedia.org/wikipedia/commons/1/12/2009-09-25_Hericium_coralloides_(Scop.)_Pers_58068_crop.jpg'>Wikimedia<\/a><br> <b>Taxa<\/b>: 1 , <b>Seq<\/b>: 5566 <b>, Sam<\/b>: 6<\/p>"],"caption_valign":"bottom","caption_halign":"left","link":[true],"h":"40px","w":"80px","gap":"5px","border_radius":"0px","layout":"grid","shuffle":false},"evals":[],"jsHooks":[]}
tax_photos_pq(
  taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
  gallery = TRUE,
  layout = "rhombus"
)
#> ■■■■■■■■■■■■■■■■                  50% | ETA:  0s
#> ℹ 1/2 - Downloading photo of Xylodon flaviporus
#> ■■■■■■■■■■■■■■■■                  50% | ETA:  0s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ 2/2 - Downloading photo of Basidiodendron eyrei
#> ✔ Photo download summary:/n
#> - 2 photos found and downloaded/n
#> - 0 taxonomic names not found
#> ℹ Creating captions for gallery

{"x":{"path":["https://svampe.databasen.org/uploads/2018-9241204_HyxAyEUOSG.JPG","http://www.mycokey.com/MycoKeySolidState/pictures/basi/cort/Basidiodendron/eyre14L.jpg"],"caption":["<p style='font-size:12px'> <b>Xylodon flaviporus<\/b><br> <\/p>","<p style='font-size:12px'> <b>Basidiodendron eyrei<\/b><br> <\/p>"],"caption_valign":"bottom","caption_halign":"left","link":[true],"h":null,"w":null,"gap":"5px","border_radius":"0px","layout":"rhombus","shuffle":false},"evals":[],"jsHooks":[]}
data_fungi_mini_cleanNames_photos <-
  tax_photos_pq(data_fungi_mini_cleanNames)
#> ℹ 1/23 - No photo available for Stereum ostrea
#> ℹ 2/23 - No photo available for Xylodon raduloides
#> ℹ 3/23 - Downloading photo of Stereum hirsutum
#> ℹ 4/23 - No photo available for Trametopsis brasiliensis
#> ℹ 5/23 - Downloading photo of Basidiodendron eyrei
#> ℹ 6/23 - No photo available for Sistotrema oblongisporum
#> ℹ 7/23 - Downloading photo of Fomes fomentarius
#> ℹ 8/23 - Downloading photo of Mycena renati
#> ℹ 9/23 - No photo available for Helicogloea pellucida
#> ℹ 10/23 - Downloading photo of Radulomyces molaris
#> ℹ 11/23 - No photo available for Elmerina caryae
#> ℹ 12/23 - No photo available for Phanerochaete livescens
#> ℹ 13/23 - Downloading photo of Gloeohypochnicium analogum
#> ℹ 14/23 - Downloading photo of Hyphoderma roseocremeum
#> ℹ 15/23 - Downloading photo of Hyphoderma setigerum
#> ℹ 16/23 - Downloading photo of Trametes versicolor
#> ℹ 17/23 - No photo available for Peniophora versiformis
#> ℹ 18/23 - Downloading photo of Exidia glandulosa
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  0s
#> ℹ 19/23 - Downloading photo of Peniophorella pubera
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  0s

#> ℹ 20/23 - Downloading photo of Auricularia mesenterica
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  0s

#> ℹ 21/23 - No photo available for Laetisaria buckii
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  0s

#> ℹ 22/23 - Downloading photo of Hericium coralloides
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  0s

#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> ℹ 23/23 - Downloading photo of Xylodon flaviporus
#> ✔ Photo download summary:/n
#> - 14 photos found and downloaded/n
#> - 18 taxa depicted/n
#> - 9 taxonomic names not found/n
#> - 27 taxa have no photo URL

# Which photo(s) depicted more than one OTU
data_fungi_mini_cleanNames_photos@tax_table[, "photo_url"] |>
  table() |>
  (\(tab) tab[as.numeric(tab) > 1])()
#> 
#> http://www.mycokey.com/MycoKeySolidState/pictures/basi/hete/Exidia/glan10L.jpg 
#>                                                                              2 
#>                                https://images.naturalis.nl/original/191145.jpg 
#>                                                                              4 
```
