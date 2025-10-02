#' Find photos of taxa from GBIF or Wikitaxa
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
#' created using  the function [pixture::pixgallery()].
#' @param overwrite_folder (logical, default FALSE) If TRUE, the folder
#'  specified in the parameter folder_name will be deleted if it already exists.
#' @param col_name_url (default "photo_url") Name of the new column in the
#'  tax_table
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param caption_valign (character, default "bottom")
#'   Vertical alignment of the caption in the gallery.
#' @param caption_font_size (int) Size of the caption font in the gallery.
#' @param simple_caption (logical, default FALSE) If TRUE, the caption of
#' the gallery photo will be only the taxonomic name. If FALSE, the caption
#' include information from the phyloseq object (number of sequences, taxa
#' and samples).
#' @param ... Other parameters to be passed to pixture::pixgallery() function.
#'
#' @returns There is three behavior.(i) If gallery = TRUE, a html gallery is
#'  created using  the function [pixture::pixgallery()].
#'  (ii) If add_to_phyloseq = TRUE, a new phyloseq object is returned
#'  with a new column (called with the parameter
#'  col_name_url) in the tax_table containing the URL. (iii) If both
#'  gallery = FALSE and add_to_phyloseq = FALSE, photos are downloaded in a
#'  folder (folder_name parameter) and the list of url are returned in the
#'  form of a tibble.
#' @export
#' @author Adrien Taudière
#' @examples
#'
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
#'
#' tax_photos_pq(data_fungi_mini_cleanNames,
#'   gallery = TRUE,
#'   h = "40px",
#'   w = "80px",
#'   source = "wikitaxa"
#' )
#'
#' tax_photos_pq(
#'   taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
#'   gallery = TRUE,
#'   layout = "rhombus"
#' )
#'
#' data_fungi_mini_cleanNames_photos <-
#'   tax_photos_pq(data_fungi_mini_cleanNames)
#'
#' # Which photo(s) depicted more than one OTU
#' data_fungi_mini_cleanNames_photos@tax_table[, "photo_url"] |>
#'   table() |>
#'   (\(tab) tab[as.numeric(tab) > 1])()
#'
tax_photos_pq <- function(physeq = NULL,
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
                          ...) {
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}, not both")
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }

  # Set default for add_to_phyloseq based on input type
  if (is.null(add_to_phyloseq)) {
    add_to_phyloseq <- !is.null(physeq)
  }

  if (!is.null(taxnames) && add_to_phyloseq) {
    cli::cli_abort("{.arg add_to_phyloseq} cannot be TRUE when {.arg taxnames} is provided")
  }

  # Check for column name collisions and handle col_prefix
  if (!is.null(physeq) && add_to_phyloseq) {
    final_col_name <- paste0(col_prefix, col_name_url)
    if (sum(colnames(physeq@tax_table) %in% final_col_name) > 0 && is.null(col_prefix)) {
      cli::cli_warn(c(
        "Column name already exists in tax_table: {.val {final_col_name}}",
        "i" = "Adding prefix 'photo_' to avoid conflicts"
      ))
      col_prefix <- "photo_"
    }
  }

  if (is.null(taxnames)) {
    taxnames_raw <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  } else {
    taxnames_raw <- taxnames
  }

  if (source == "gbif") {
    gbif_taxa <- rgbif::name_backbone_checklist(taxnames_raw) |>
      filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
      distinct()
    taxnames <- gbif_taxa$canonicalName
  } else if (source == "wikitaxa") {
    check_package("wikitaxa")
    taxnames <- taxnames_raw
  } else {
    cli::cli_abort("Source parameter allows only {.val gbif} or {.val wikitaxa} values")
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
      xs_gbif <- suppressWarnings(rgbif::name_usage(gbif_taxa$usageKey[gbif_taxa$canonicalName == taxnames[i]], data = "media")$data$identifier[[1]])

      if (is.null(xs_gbif)) {
        photo_url[i] <- NA
        if (verbose) {
          cli::cli_alert_info("{.val {i}}/{.val {length(taxnames)}} - No photo available for {.emph {taxnames[i]}}")
        }
      } else {
        if (verbose) {
          cli::cli_alert_info("{.val {i}}/{.val {length(taxnames)}} - Downloading photo of {.emph {taxnames[i]}}")
        }
        photo_url[i] <- xs_gbif
      }
    } else if (source == "wikitaxa") {
      xs_wt <- tryCatch(wikitaxa::wt_data(taxnames[i], property = c("P225", "P18")),
        error = function(e) NULL
      )
      if (sum(xs_wt$claims$property_value == "image") > 0) {
        if (verbose) {
          cli::cli_alert_info("{.val {i}}/{.val {length(taxnames)}} - Downloading photo of {.emph {taxnames[i]}}")
        }

        photo_names <- xs_wt$claims |>
          filter(property_value == "image") |>
          pull(value) |>
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
          cli::cli_alert_info("{.val {i}}/{.val {length(taxnames)}} - No photo available for {.emph {taxnames[i]}}")
        }
      }
    }
  }

  # Complete progress bar
  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  photo_url_tib <- cbind(photo_url, taxnames) |>
    as_tibble()

  colnames(photo_url_tib) <- c(col_name_url, "taxa_name")
  
  # Apply col_prefix to the photo URL column
  if (!is.null(col_prefix)) {
    photo_url_tib <- photo_url_tib |>
      rename_with(~ paste0(col_prefix, .), .cols = -taxa_name)
  }
  
  final_col_name <- paste0(col_prefix, col_name_url)

  if (!is.null(physeq)) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")
    new_physeq@tax_table <-
      left_join(tax_tab, photo_url_tib, by = join_by(taxa_name)) |>
      as.matrix() |>
      tax_table()

    rownames(new_physeq@tax_table) <- taxa_names(physeq)
  }

  if (verbose) {
    photos_found <- sum(!is.na(photo_url))
    names_not_found <- sum(is.na(photo_url))

    if (!is.null(physeq)) {
      taxa_depicted <- sum(!is.na(new_physeq@tax_table[, final_col_name]))
      taxa_no_photo <- sum(is.na(new_physeq@tax_table[, final_col_name]))

      cli::cli_bullets(c(
        "v" = "Photo download summary:/n",
        "  • {.val {photos_found}} photos found and downloaded/n",
        "  • {.val {taxa_depicted}} taxa depicted/n",
        "  • {.val {names_not_found}} taxonomic names not found/n",
        "  • {.val {taxa_no_photo}} taxa have no photo URL"
      ))
    } else {
      cli::cli_bullets(c(
        "v" = "Photo download summary:/n",
        "  • {.val {photos_found}} photos found and downloaded/n",
        "  • {.val {names_not_found}} taxonomic names not found"
      ))
    }
  }

  if (add_to_phyloseq && !gallery) {
    return(new_physeq)
  } else if (gallery) {
    if (verbose) {
      cli::cli_alert_info("Creating captions for gallery")
    }
    for (i in seq_along(taxnames)) {
      if (simple_caption || is.null(physeq)) {
        captions[i] <- paste(
          paste0("<p style='font-size:", caption_font_size, "px'>"),
          paste0("<b>", taxnames[i], "</b><br>"), "</p>"
        )
      } else {
        tax_tab_gallery <- as.data.frame(new_physeq@tax_table)
        captions[i] <- paste(
          paste0("<p style='font-size:", caption_font_size, "px'>"),
          paste0("<b>", taxnames[i], "</b><br>"),
          paste0(
            "<b>Source</b>: <a href='",
            photo_url[i],
            "'>",
            "Wikimedia",
            "</a><br>"
          ),
          paste0(
            "<b>Taxa</b>: ",
            sum(tax_tab_gallery[, "taxa_name"] %in%
              taxnames[i])
          ),
          paste0(
            ", <b>Seq</b>: ",
            sum(taxa_sums(new_physeq)[tax_tab_gallery[, "taxa_name"] %in%
              taxnames[i]])
          ),
          paste0(
            "<b>, Sam</b>: ",
            sum(sample_sums(subset_taxa_pq(new_physeq, new_physeq@tax_table[, "taxa_name"] == taxnames[i], verbose = FALSE, clean_pq = FALSE)) > 0),
            "</p>"
          )
        )
      }
    }
    check_package("pixture")
    pixture::pixgallery(photo_url[!is.na(photo_url)], caption = captions[!is.na(photo_url)], caption_valign = caption_valign, ...)
  } else {
    if (overwrite_folder) {
      unlink(folder_name, recursive = TRUE)
    }

    if (dir.exists(folder_name)) {
      stop(
        "The folder ",
        folder_name,
        " already exist. You may want to use an
           other folder_name or set overwrite_folder to TRUE."
      )
    }

    dir.create(folder_name)
    download.file(photo_url[!is.na(photo_url)], paste0(folder_name, "/", taxnames[!is.na(photo_url)], ".jpg"), quiet = TRUE)
    return(invisible(photo_url))
  }
}
