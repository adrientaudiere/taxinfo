#' Find photos of taxa from GBIF or Wikitaxa
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' @details
#'  There is three behavior. See the returns section. Gbif source is quicker
#'  than wikitaxa source. Note that for the moment the function only return
#'  one photo per species.
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default = "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. the c("Genus", "Species")).
#' @param source (Character) either "gbif" or "wikitaxa".
#' @param folder_name (default "photos_physeq") Name of the folder where photos
#' will be downloaded. Only used if both add_to_phyloseq and gallery are FALSE.
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided)
#'  If TRUE, a new phyloseq object is returned with a new column containing the URL
#'  (entitled with the parameter col_name_url) in the tax_table.
#'  Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#'  Cannot be TRUE if `taxnames` is provided.
#' @param col_prefix A character string to be added as a prefix to the new
#' columns names added to the tax_table slot of the phyloseq object (default: NULL).
#' @param gallery (logical, default FALSE) If TRUE, a html gallery is
#' created using [htmltools::browsable()].
#' @param overwrite_folder (logical, default FALSE) If TRUE, the folder
#'  specified in the parameter folder_name will be deleted if it already exists.
#' @param col_name_url (default "photo_url") Name of the new column in the
#'  tax_table
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param caption_valign (character, default "bottom")
#'   Vertical alignment of the caption in the gallery. Either `"bottom"` or
#'   `"top"`.
#' @param caption_font_size (int) Size of the caption font in the gallery.
#' @param simple_caption (logical, default FALSE) If TRUE, the caption of
#' the gallery photo will be only the taxonomic name. If FALSE, the caption
#' include information from the phyloseq object (number of sequences, taxa
#' and samples).
#' @param img_height (character, default "150px") Height of images in the gallery.
#' @param img_width (character, default "200px") Width of images in the gallery.
#' @param ... Unused, kept for backward compatibility.
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns There is three behavior.(i) If add_to_phyloseq = TRUE, a new
#'  phyloseq object is returned with a new column (called with the parameter
#'  col_name_url) in the tax_table containing the URL; the gallery is printed
#'  as a side-effect if `gallery = TRUE`. (ii) If add_to_phyloseq = FALSE and
#'  gallery = TRUE, the HTML gallery is returned. (iii) If both gallery = FALSE
#'  and add_to_phyloseq = FALSE, photos are downloaded in a folder
#'  (folder_name parameter) and the list of url are returned in the form of
#'  a tibble.
#' @importFrom dplyr filter distinct pull rename_with left_join as_tibble join_by
#' @export
#' @author Adrien Taudiere
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
#'
#' tax_photos_pq(data_fungi_mini_cleanNames,
#'   gallery = TRUE,
#'   img_height = "40px",
#'   img_width = "80px",
#'   source = "wikitaxa"
#' )
#'
#' tax_photos_pq(
#'   taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
#'   gallery = TRUE
#' )
#'
#' data_fungi_mini_cleanNames_photos <-
#'   tax_photos_pq(data_fungi_mini_cleanNames)
#'
#' # Which photo(s) depicted more than one OTU
#' data_fungi_mini_cleanNames_photos@tax_table[, "photo_url"] |>
#'   table() |>
#'   (\(tab) tab[as.numeric(tab) > 1])()
#' }
#'
tax_photos_pq <- function(
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
) {
  resolved <- resolve_taxa_input(
    physeq = physeq,
    taxnames = taxnames,
    add_to_phyloseq = add_to_phyloseq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = discard_genus_alone,
    discard_NA = discard_NA
  )
  taxnames_raw <- resolved$taxnames
  add_to_phyloseq <- resolved$add_to_phyloseq

  # Check for column name collisions and handle col_prefix
  if (!is.null(physeq) && add_to_phyloseq) {
    final_col_name <- paste0(col_prefix, col_name_url)
    if (
      sum(colnames(physeq@tax_table) %in% final_col_name) > 0 &&
        is.null(col_prefix)
    ) {
      cli::cli_warn(c(
        "Column name already exists in tax_table: {.val {final_col_name}}",
        "i" = "Adding prefix 'photo_' to avoid conflicts"
      ))
      col_prefix <- "photo_"
    }
  }

  if (source == "gbif") {
    gbif_taxa <- rgbif::name_backbone_checklist(taxnames_raw)
    gbif_taxa$query_name <- taxnames_raw
    gbif_taxa <- gbif_taxa |>
      dplyr::filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
      dplyr::distinct()
    taxnames <- gbif_taxa$canonicalName
  } else if (source == "wikitaxa") {
    check_package("wikitaxa")
    taxnames <- taxnames_raw
  } else {
    cli::cli_abort(
      "Source parameter allows only {.val gbif} or {.val wikitaxa} values"
    )
  }

  photo_url <- rep(NA, length(taxnames))
  captions <- rep(NA, length(taxnames))

  if (verbose) {
    pb <- cli::cli_progress_bar(total = length(taxnames))
  }

  for (i in seq_along(taxnames)) {
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
    }

    if (source == "gbif") {
      # select only the first photo for each species
      xs_gbif <- suppressWarnings(rgbif::name_usage(
        gbif_taxa$usageKey[gbif_taxa$canonicalName == taxnames[i]],
        data = "media"
      )$data$identifier[[1]])

      if (is.null(xs_gbif)) {
        photo_url[i] <- NA
        if (verbose) {
          cli::cli_alert_info(
            "{.val {i}}/{.val {length(taxnames)}} - No photo available for {.emph {taxnames[i]}}"
          )
        }
      } else {
        if (verbose) {
          cli::cli_alert_info(
            "{.val {i}}/{.val {length(taxnames)}} - Downloading photo of {.emph {taxnames[i]}}"
          )
        }
        photo_url[i] <- xs_gbif
      }
    } else if (source == "wikitaxa") {
      xs_wt <- tryCatch(
        wikitaxa::wt_data(taxnames[i], property = c("P225", "P18")),
        error = function(e) NULL
      )
      if (!is.null(xs_wt) && sum(xs_wt$claims$property_value == "image") > 0) {
        if (verbose) {
          cli::cli_alert_info(
            "{.val {i}}/{.val {length(taxnames)}} - Downloading photo of {.emph {taxnames[i]}}"
          )
        }

        photo_names <- xs_wt$claims |>
          dplyr::filter(property_value == "image") |>
          dplyr::pull(value) |>
          gsub(pattern = " ", replacement = "_") |>
          stringr::str_split_1(",")

        # select only the first photo for each species
        photo_name <- photo_names[[1]]

        check_package("digest")
        md5 <- digest::digest(photo_name, algo = "md5", serialize = FALSE)
        photo_url[i] <- paste0(
          "https://upload.wikimedia.org/wikipedia/commons/",
          substr(md5, 1, 1),
          "/",
          substr(md5, 1, 2),
          "/",
          photo_name
        )
      } else {
        photo_url[i] <- NA
        if (verbose) {
          cli::cli_alert_info(
            "{.val {i}}/{.val {length(taxnames)}} - No photo available for {.emph {taxnames[i]}}"
          )
        }
      }
    }
  }

  # Complete progress bar
  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  # For GBIF use the original query name as join key so it matches tax_table
  # entries built from raw column values; for wikitaxa taxnames == taxnames_raw.
  join_taxa_names <- if (source == "gbif") gbif_taxa$query_name else taxnames

  photo_url_tib <- data.frame(
    photo_url,
    join_taxa_names,
    stringsAsFactors = FALSE
  ) |>
    dplyr::as_tibble()

  colnames(photo_url_tib) <- c(col_name_url, "taxa_name")

  # Apply col_prefix to the photo URL column
  if (!is.null(col_prefix)) {
    photo_url_tib <- photo_url_tib |>
      dplyr::rename_with(~ paste0(col_prefix, .), .cols = -taxa_name)
  }

  final_col_name <- paste0(col_prefix, col_name_url)

  if (!is.null(physeq)) {
    new_physeq <- augment_tax_table(
      physeq,
      photo_url_tib,
      taxonomic_rank = taxonomic_rank
    )
  }

  if (verbose) {
    if (!is.null(physeq)) {
      cli::cli_bullets(c(
        "v" = "Photo download summary:",
        " " = "{.val {sum(!is.na(photo_url))}} photos found",
        " " = "{.val {sum(!is.na(new_physeq@tax_table[, final_col_name]))}} taxa depicted",
        " " = "{.val {sum(is.na(photo_url))}} taxonomic names not found",
        " " = "{.val {sum(is.na(new_physeq@tax_table[, final_col_name]))}} taxa have no photo URL"
      ))
    } else {
      cli::cli_bullets(c(
        "v" = "Photo download summary:",
        " " = "{.val {sum(!is.na(photo_url))}} photos found",
        " " = "{.val {sum(is.na(photo_url))}} taxonomic names not found"
      ))
    }
  }

  # Build gallery if requested
  if (gallery) {
    if (verbose) {
      cli::cli_alert_info("Creating captions for gallery")
    }
    for (i in seq_along(taxnames)) {
      if (simple_caption || is.null(physeq)) {
        captions[i] <- paste0(
          "<p style='font-size:",
          caption_font_size,
          "px'>",
          "<b>",
          taxnames[i],
          "</b>",
          "</p>"
        )
      } else {
        tax_tab_gallery <- as.data.frame(new_physeq@tax_table)
        taxa_match <- setNames(
          tax_tab_gallery[, "taxa_name"] %in% taxnames[i],
          taxa_names(new_physeq)
        )
        captions[i] <- paste0(
          "<p style='font-size:",
          caption_font_size,
          "px'>",
          "<b>",
          taxnames[i],
          "</b><br>",
          "<b>Source</b>: <a href='",
          photo_url[i],
          "'>Wikimedia</a><br>",
          "<b>Taxa</b>: ",
          sum(taxa_match),
          ", <b>Seq</b>: ",
          sum(taxa_sums(new_physeq)[taxa_match]),
          ", <b>Sam</b>: ",
          sum(
            sample_sums(subset_taxa_pq(
              new_physeq,
              taxa_match,
              verbose = FALSE,
              clean_pq = FALSE
            )) >
              0
          ),
          "</p>"
        )
      }
    }

    valid <- !is.na(photo_url)
    gallery_out <- .make_photo_gallery(
      urls = photo_url[valid],
      captions = captions[valid],
      caption_valign = caption_valign,
      img_height = img_height,
      img_width = img_width
    )

    if (add_to_phyloseq) {
      print(gallery_out)
      return(invisible(new_physeq))
    } else {
      return(gallery_out)
    }
  }

  if (add_to_phyloseq) {
    return(new_physeq)
  }

  # Download photos to folder
  if (overwrite_folder) {
    unlink(folder_name, recursive = TRUE)
  }

  if (dir.exists(folder_name)) {
    cli::cli_abort(c(
      "The folder {.path {folder_name}} already exists.",
      "i" = "Use a different {.arg folder_name} or set {.arg overwrite_folder = TRUE}."
    ))
  }

  dir.create(folder_name)
  download.file(
    photo_url[!is.na(photo_url)],
    paste0(folder_name, "/", taxnames[!is.na(photo_url)], ".jpg"),
    quiet = TRUE
  )
  invisible(photo_url_tib)
}


#' Build a simple HTML photo gallery
#'
#' Internal helper replacing `pixture::pixgallery()`.
#'
#' @param urls Character vector of image URLs (no NAs).
#' @param captions Character vector of HTML captions (same length as urls),
#'   or NULL.
#' @param caption_valign Either `"top"` or `"bottom"`.
#' @param img_height CSS height string (e.g. `"150px"`).
#' @param img_width CSS width string (e.g. `"200px"`).
#' @noRd
.make_photo_gallery <- function(
  urls,
  captions = NULL,
  caption_valign = "bottom",
  img_height = "150px",
  img_width = "200px"
) {
  check_package("htmltools")

  items <- lapply(seq_along(urls), function(i) {
    cap_html <- if (!is.null(captions)) {
      htmltools::div(
        style = "font-size:small; word-wrap:break-word;",
        htmltools::HTML(captions[i])
      )
    } else {
      NULL
    }

    img_tag <- htmltools::tags$img(
      src = urls[i],
      style = paste0(
        "width:",
        img_width,
        ";",
        "height:",
        img_height,
        ";",
        "object-fit:cover;",
        "display:block;"
      ),
      loading = "lazy"
    )

    children <- if (caption_valign == "top") {
      list(cap_html, img_tag)
    } else {
      list(img_tag, cap_html)
    }

    htmltools::div(
      style = paste0(
        "display:inline-block;",
        "margin:6px;",
        "vertical-align:top;",
        "width:",
        img_width,
        ";",
        "border:1px solid #ddd;",
        "border-radius:4px;",
        "overflow:hidden;"
      ),
      children
    )
  })

  htmltools::browsable(
    htmltools::div(
      style = "display:flex; flex-wrap:wrap; gap:4px;",
      items
    )
  )
}
