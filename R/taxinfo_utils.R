#' Calculate Bounding Box Around a Point
#'
#' @description
#'
#' This function calculates a geographic bounding box around a given point
#' with a specified radius in kilometers.
#'
#' @param longitude Numeric. Longitude of the center point in decimal degrees.
#' @param latitude Numeric. Latitude of the center point in decimal degrees.
#' @param radius_km Numeric. Radius in kilometers for the bounding box.
#'
#' @return A list containing xmin, xmax, ymin, ymax coordinates in decimal degrees.
#'
#' @details The function uses an approximation where 1 degree ~= 111.32 km and
#'   adjusts for latitude distortion where longitude degrees get closer at the poles.
#'
#' @author Adrien Taudiere
#'
#' @keywords internal
calculate_bbox <- function(longitude = NULL, latitude = NULL, radius_km = 1) {
  # Approximation: 1 degree ~= 111.32 km
  lat_offset <- radius_km / 111.32
  # Adjustment for latitude (longitude degrees get closer at the poles)
  lon_offset <- radius_km / (111.32 * cos(latitude * pi / 180))

  if (is.null(longitude) | is.null(latitude) | is.null(radius_km)) {
    cli::cli_abort(
      "Parameters {.arg longitude}, {.arg latitude} and {.arg radius_km} must be provided"
    )
  }
  res <- list(
    "xmin" = longitude - lon_offset,
    "xmax" = longitude + lon_offset,
    "ymin" = latitude - lat_offset,
    "ymax" = latitude + lat_offset
  )

  return(res)
}


#' Text summary for a taxonomic rank
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Create a text to summarize the number of samples, taxa, sequences and occurrences of selected taxa in a phyloseq object for a given value in the column of a tax_table
#'
#' @param physeq A phyloseq object
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'  The column(s) present in the @tax_table slot of the phyloseq object. Can
#'  be a vector of two columns (e.g. c("Genus", "Species")).
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param min_nb_seq minimum number of sequences by OTUs by
#'   samples to take into count this OTUs in this sample. For example,
#'   if min_nb_seq=2,each value of 2 or less in the OTU table
#'   will not count in the venn diagram
#' @param ... Additional arguments to pass to [subset_taxa_pq()].
#' @author Adrien Taudiere
#'
#' @returns A character string summarizing the number of samples, taxa, sequences and occurrences of the selected taxa.
#' @export
#'
#' @examples
#' taxa_summary_text(data_fungi_cleanNames, taxnames = "Xylodon flaviporus")
#' taxa_summary_text(data_fungi_cleanNames,
#'   taxnames = "Xylodon flaviporus",
#'   min_nb_seq = 100, verbose = FALSE
#' )
#' taxa_summary_text(data_fungi_cleanNames,
#'   taxonomic_rank = "Trait",
#'   taxnames = c("Soft Rot"), verbose = FALSE
#' )
taxa_summary_text <- function(
  physeq,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  verbose = TRUE,
  min_nb_seq = 0,
  ...
) {
  new_physeq <- select_taxa_pq(
    physeq = physeq,
    taxonomic_rank = taxonomic_rank,
    taxnames = taxnames,
    verbose = verbose,
    clean_pq = FALSE,
    ...
  ) |>
    clean_pq(silent = TRUE)

  if (min_nb_seq > 0) {
    new_physeq@otu_table[new_physeq@otu_table < min_nb_seq] <- 0
    new_physeq2 <- clean_pq(new_physeq, silent = TRUE)
    if (verbose) {
      removed_samples <- nsamples(new_physeq) - nsamples(new_physeq2)
      removed_taxa <- ntaxa(new_physeq) - ntaxa(new_physeq2)
      removed_sequences <- sum(new_physeq@otu_table) -
        sum(new_physeq2@otu_table)
      removed_occurrences <- sum(new_physeq@otu_table > 0) -
        sum(new_physeq2@otu_table > 0)

      cli::cli_alert_info(c(
        "Filtering OTUs with less than {.val {min_nb_seq}} sequences removed:/n",
        "  - {.val {removed_samples}} samples/n",
        "  - {.val {removed_taxa}} taxa/n",
        "  - {.val {removed_sequences}} sequences/n",
        "  - {.val {removed_occurrences}} occurrences/n"
      ))
    }
    new_physeq <- new_physeq2
  }

  nsamp <- nsamples(new_physeq)
  ntaxa <- ntaxa(new_physeq)
  nseq <- sum(new_physeq@otu_table)
  noccur <- sum(new_physeq@otu_table > 0)

  paste0(
    taxnames,
    ": ",
    nsamp,
    " samp., ",
    ntaxa,
    " taxa, ",
    nseq,
    " seq., ",
    noccur,
    " occ."
  )
}


#' Check package availability and propose installation instructions
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-maturing-blue" alt="lifecycle-maturing"></a>
#'
#' This function checks if a package is available using requireNamespace.
#' If the package is not available, it provides helpful installation instructions.
#'
#' @param package (required) Character string. Name of the package to check.
#' @param repo Character string. Repository source for installation suggestion.
#'        Options: "CRAN" (default), "Bioconductor", "GitHub".
#' @param github_repo Character string. GitHub repository
#'        in format "username/repository". It overrides repo if provided.
#'        Required if repo is "GitHub".
#' @param stop_on_error Logical. If TRUE  (default), stops execution when package
#'    is missing. If FALSE, returns FALSE and shows message.
#' @param quietly Logical. If TRUE, suppresses the requireNamespace loading messages.
#'        Default is TRUE.
#'
#' @return Logical. TRUE if package is available, FALSE if not available.
#'
#' @examples
#' \dontrun{
#' # Check CRAN package
#' check_package("dplyr")
#'
#' # Check Bioconductor package
#' check_package("Biostrings", repo = "Bioconductor")
#'
#' # Check GitHub package
#' check_package("MiscMetabar",
#'   repo = "GitHub",
#'   github_repo = "adrientaudiere/MiscMetabar"
#' )
#'
#' # Stop execution if package is missing
#' check_package("ggplot2", stop_on_error = TRUE)
#' }
#' @export
check_package <- function(
  package,
  repo = "CRAN",
  github_repo = NULL,
  stop_on_error = TRUE,
  quietly = TRUE
) {
  # Validate inputs
  if (!is.character(package) || length(package) != 1) {
    cli::cli_abort("'{.arg package}' must be a single character string")
  }

  if (!is.null(github_repo)) {
    repo <- "GitHub"
  }

  if (!repo %in% c("CRAN", "Bioconductor", "GitHub") && is.null(github_repo)) {
    if (!is.character(repo)) {
      cli::cli_abort(
        "'{.arg repo}' must be one of {.val CRAN}, {.val Bioconductor}, {.val GitHub}"
      )
    }
  }

  # Check if package is available
  is_available <- requireNamespace(package, quietly = quietly)

  if (!is_available) {
    # Create installation message based on repository
    install_msg <- switch(
      repo,
      "CRAN" = paste0('install.packages("', package, '")'),
      "Bioconductor" = paste0(
        'if (!requireNamespace("BiocManager")) {\n',
        '  install.packages("BiocManager")\n',
        "}\n",
        'BiocManager::install("',
        package,
        '")'
      ),
      "GitHub" = {
        if (is.null(github_repo)) {
          cli::cli_abort(
            "For GitHub packages, '{.arg github_repo}' must be specified as {.val username/repository}"
          )
        }
        paste0(
          'if (!requireNamespace("devtools")) {\n',
          '  install.packages("devtools")\n',
          "}\n",
          'devtools::install_github("',
          github_repo,
          '")'
        )
      },
    )

    if (stop_on_error) {
      cli::cli_abort(c(
        "Package {.pkg {package}} is required but not installed.",
        "i" = "To install it, run:",
        " " = "{.code {install_msg}}"
      ))
    } else {
      cli::cli_alert_info(c(
        "Package {.pkg {package}} is required but not installed.",
        "i" = "To install it, run:",
        " " = "{.code {install_msg}}"
      ))
    }
  }

  return(is_available)
}


#' Load WWF/TNC terrestrial ecoregions as an `sf` object
#'
#' @description
#' Internal helper that returns the terrestrial ecoregions polygon layer used by
#' [tax_ecoregion_occur()], [tax_check_ecoregion()] and
#' [points_to_ecoregions()]. The layer is read from the shapefile shipped with
#' the package (`inst/extdata/downloads/eco_terra/tnc_terr_ecoregions.shp`) and
#' cached in a package-internal environment so that repeated calls are free.
#'
#' @param ecoreg_name (character, default `"eco_terra"`). Currently only
#' `"eco_terra"` is supported; the argument is kept for future extension.
#' @param refresh (logical, default `FALSE`). If `TRUE`, force a re-read from
#'  disk and refresh the cache.
#'
#' @returns An `sf` object with valid geometries and at least the columns
#' `ECO_NAME`, `BIOME` (or `WWF_MHTNAM`) and `REALM` (or `WWF_REALM2`).
#'
#' @author Adrien Taudiere
#' @keywords internal
load_ecoregions <- function(ecoreg_name = "eco_terra", refresh = FALSE) {
  cache_key <- paste0("ecoregions_", ecoreg_name)
  cache <- get(".taxinfo_cache", envir = asNamespace("taxinfo"))

  if (!refresh && exists(cache_key, envir = cache, inherits = FALSE)) {
    return(get(cache_key, envir = cache, inherits = FALSE))
  }

  shp <- system.file(
    "extdata",
    "downloads",
    "eco_terra",
    "tnc_terr_ecoregions.shp",
    package = "taxinfo"
  )

  if (nzchar(shp) && file.exists(shp)) {
    ecoregions <- sf::read_sf(shp) |>
      sf::st_make_valid()
  } else {
    check_package("gbif.range")
    # The ~50 MB ecoregion layer is not bundled with the installed package, so
    # download it once into a stable per-user cache (never the working
    # directory: gbif.range's default save_dir is `getwd()/inst/extdata`).
    save_dir <- tools::R_user_dir("taxinfo", "cache")
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    if (!dir.exists(file.path(save_dir, ecoreg_name))) {
      cli::cli_inform(c(
        "i" = "Ecoregion layer {.val {ecoreg_name}} is not bundled with {.pkg taxinfo}.",
        " " = "Downloading it once (~50 MB) to {.path {save_dir}}."
      ))
    }
    gbif.range::check_and_get_ecoreg(ecoreg_name, save_dir = save_dir)
    ecoregions <- gbif.range::read_ecoreg(
      ecoreg_name = ecoreg_name,
      save_dir = save_dir
    ) |>
      sf::st_as_sf() |>
      sf::st_make_valid()
  }

  assign(cache_key, ecoregions, envir = cache)
  ecoregions
}
