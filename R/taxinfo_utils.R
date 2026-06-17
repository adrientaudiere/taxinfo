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
#' data_fungi_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
#'
#' taxa_summary_text(data_fungi_cleanNames, taxnames = "Xylodon flaviporus")
#' \donttest{
#' taxa_summary_text(data_fungi_cleanNames,
#'   taxnames = "Xylodon flaviporus",
#'   min_nb_seq = 100, verbose = FALSE
#' )
#' taxa_summary_text(data_fungi_cleanNames,
#'   taxonomic_rank = "Trait",
#'   taxnames = c("Soft Rot"), verbose = FALSE
#' )
#' }
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


#' Are GBIF credentials available?
#'
#' @description
#' Non-throwing predicate that returns `TRUE` when all three GBIF credential
#' environment variables (`GBIF_USER`, `GBIF_PWD`, `GBIF_EMAIL`) are set to a
#' non-empty value. Single source of truth used by [check_gbif_credentials()]
#' and by the test helper `skip_if_no_gbif_credentials()`.
#'
#' @returns A logical scalar.
#'
#' @author Adrien Taudiere
#' @keywords internal
has_gbif_credentials <- function() {
  Sys.getenv("GBIF_USER") != "" &&
    Sys.getenv("GBIF_PWD") != "" &&
    Sys.getenv("GBIF_EMAIL") != ""
}


#' Abort if GBIF credentials are missing
#'
#' @description
#' Internal helper that stops with an informative message (registration link and
#' `.Renviron` guidance) when the GBIF credentials required by the Download API
#' are not set. Used by [gbif_download()] and every function that relies on
#' `rgbif::occ_download()` / `rgbif::occ_download_sql()`.
#'
#' @returns Invisibly `TRUE` when credentials are available; otherwise aborts.
#'
#' @author Adrien Taudiere
#' @keywords internal
check_gbif_credentials <- function() {
  if (!has_gbif_credentials()) {
    cli::cli_abort(c(
      "GBIF credentials are required for the Download API.",
      "i" = "Please set the following in your {.file .Renviron} file:",
      " " = "GBIF_USER, GBIF_PWD, GBIF_EMAIL",
      "i" = "Register at: {.url https://www.gbif.org/user/register}",
      "i" = "See: {.url https://docs.ropensci.org/rgbif/articles/gbif_credentials.html}"
    ))
  }
  invisible(TRUE)
}


#' Run a GBIF download and import the result
#'
#' @description
#' Internal helper that wraps the full asynchronous GBIF Download API lifecycle
#' (submit, wait, get, import, clean up) in a single call. It accepts either a
#' set of predicates (forwarded to [rgbif::occ_download()]) or a SQL query (sent
#' to [rgbif::occ_download_sql()]). GBIF credentials are required; see
#' [check_gbif_credentials()].
#'
#' @param ... Predicates built with [rgbif::pred()], [rgbif::pred_in()], etc.
#'  Passed to [rgbif::occ_download()]. Ignored when `sql` is supplied.
#' @param sql (character, default `NULL`). A SQL query string. When supplied, the
#'  download is submitted with [rgbif::occ_download_sql()] (server-side filtering
#'  and `LIMIT`) instead of predicates. The SQL Download API is a gated preview;
#'  the account must be enabled for it.
#' @param format (character, default `"SIMPLE_CSV"`). Download format passed to
#'  [rgbif::occ_download()]. Ignored when `sql` is supplied.
#' @param verbose (logical, default `TRUE`). If `TRUE`, print progress messages.
#'
#' @returns A tibble of imported occurrence records. The download key and DOI are
#'  attached as `attr(x, "key")` and `attr(x, "doi")` for citation.
#'
#' @author Adrien Taudiere
#' @seealso [rgbif::occ_download()], [rgbif::occ_download_sql()],
#'  [check_gbif_credentials()]
#' @keywords internal
gbif_download <- function(
  ...,
  sql = NULL,
  format = "SIMPLE_CSV",
  verbose = TRUE
) {
  check_gbif_credentials()

  download_key <- tryCatch(
    {
      if (is.null(sql)) {
        rgbif::occ_download(..., format = format)
      } else {
        rgbif::occ_download_sql(sql)
      }
    },
    error = function(e) {
      if (!is.null(sql) && grepl("sql", e$message, ignore.case = TRUE)) {
        cli::cli_abort(c(
          "Failed to submit GBIF SQL download request.",
          "i" = "The SQL Download API is a gated preview; your GBIF account must be enabled for it.",
          "i" = "Request access: {.url https://techdocs.gbif.org/en/data-use/api-sql-downloads}",
          "x" = "Error: {e$message}"
        ))
      }
      cli::cli_abort(c(
        "Failed to submit GBIF download request.",
        "i" = "GBIF credentials are required. Please ensure you have set:",
        " " = "GBIF_USER, GBIF_PWD, GBIF_EMAIL in your .Renviron file",
        "i" = "Register at: {.url https://www.gbif.org/user/register}",
        "i" = "See: {.url https://docs.ropensci.org/rgbif/articles/gbif_credentials.html}",
        "x" = "Error: {e$message}"
      ))
    }
  )

  if (verbose) {
    cli::cli_alert_info("Download key: {.val {download_key}}")
    cli::cli_alert_info(
      "Waiting for download to complete (this may take a few minutes)..."
    )
  }

  # The download is already submitted (and counted against the GBIF quota) at
  # this point. If the wait/get/import phase fails - typically a transient
  # network timeout while polling api.gbif.org - surface the key so the user can
  # resume without re-submitting.
  occ_data <- tryCatch(
    {
      rgbif::occ_download_wait(download_key, quiet = !verbose)
      if (verbose) {
        cli::cli_alert_success("Download complete. Importing data...")
      }
      download_path <- rgbif::occ_download_get(download_key, overwrite = TRUE)
      d <- rgbif::occ_download_import(download_path)
      file.remove(download_path)
      d
    },
    error = function(e) {
      cli::cli_abort(c(
        "GBIF download {.val {download_key}} was submitted but could not be retrieved.",
        "i" = "This is usually a transient network issue; the download is still prepared on GBIF's servers.",
        "i" = "Resume it later (no need to re-submit) with:",
        " " = "{.code rgbif::occ_download_get(\"{download_key}\", overwrite = TRUE) |> rgbif::occ_download_import()}",
        "x" = "Error: {conditionMessage(e)}"
      ))
    }
  )

  attr(occ_data, "key") <- as.character(download_key)
  attr(occ_data, "doi") <- attr(download_key, "doi")
  occ_data
}


#' Attribute downloaded GBIF records to the queried taxa
#'
#' @description
#' Internal helper that tags each record of a GBIF download with the queried
#' taxon it belongs to. A predicate download with `pred_in("taxonKey", keys)` is
#' *hierarchical*: it returns a taxon and all its descendants, whose own
#' `taxonKey` is more specific than the queried key. A naive equality join on
#' `taxonKey` therefore drops infraspecific records (and every record of a
#' higher-rank query). Records are attributed by membership instead: a record
#' belongs to queried key `K` when its `taxonKey` *or* `speciesKey` equals `K`;
#' as a fallback for higher-rank matches, the queried `canonicalName` is matched
#' against the record's taxonomic name columns (`species`, `genus`, `family`, …).
#'
#' @param occ_data (data frame) Imported GBIF occurrences (SIMPLE_CSV schema).
#' @param gbif_taxa (tibble) Resolved taxa with `usageKey`, `canonicalName` and
#'  `verbatim_name`.
#'
#' @returns `occ_data` with two added columns, `taxon_name` (the queried
#'  `verbatim_name`) and `usageKey` (the queried key). Records may be duplicated
#'  if they match more than one queried taxon.
#'
#' @author Adrien Taudiere
#' @keywords internal
attribute_gbif_records <- function(occ_data, gbif_taxa) {
  name_cols <- intersect(
    c("species", "genus", "family", "order", "class", "phylum", "kingdom"),
    names(occ_data)
  )

  out <- vector("list", nrow(gbif_taxa))
  for (i in seq_len(nrow(gbif_taxa))) {
    key <- gbif_taxa$usageKey[i]
    canonical <- gbif_taxa$canonicalName[i]

    sel <- rep(FALSE, nrow(occ_data))
    if ("speciesKey" %in% names(occ_data)) {
      sel <- sel | (occ_data$speciesKey == key)
    }
    if ("taxonKey" %in% names(occ_data)) {
      sel <- sel | (occ_data$taxonKey == key)
    }
    # Higher-rank fallback: match the queried name against name columns.
    if (!isTRUE(any(sel, na.rm = TRUE)) && length(name_cols) > 0) {
      for (nc in name_cols) {
        sel <- sel | (occ_data[[nc]] == canonical)
      }
    }

    rows <- which(sel)
    if (length(rows) > 0) {
      d_i <- occ_data[rows, , drop = FALSE]
      d_i$taxon_name <- gbif_taxa$verbatim_name[i]
      d_i$usageKey <- key
      out[[i]] <- d_i
    }
  }

  bind_rows(out)
}


#' Compute occurrence statistics around a point
#'
#' @description
#' Pure (network-free) helper used by [tax_occur_check()] and its batched
#' wrappers. Given a data frame of occurrences with `decimalLongitude` /
#' `decimalLatitude` columns and a reference point, it computes the distance of
#' each occurrence to the point and summarises how many fall within `radius_km`.
#'
#' @param occ_df (data frame) Occurrences with `decimalLongitude` and
#'  `decimalLatitude` columns. May be `NULL` or empty.
#' @param longitude,latitude (numeric) Reference point in decimal degrees.
#' @param radius_km (numeric) Search radius in kilometres.
#' @param circle_form (logical, default `TRUE`). If `TRUE`, keep only
#'  occurrences within `radius_km` of the point (circular area); if `FALSE`, all
#'  occurrences in `occ_df` are counted.
#'
#' @returns A list with `count_in_radius`, `closest_distance_km`,
#'  `mean_distance_km`, `closest_point_lat`, `closest_point_lon` and the
#'  (filtered) `occ_data`.
#'
#' @author Adrien Taudiere
#' @keywords internal
compute_occur_stats <- function(
  occ_df,
  longitude,
  latitude,
  radius_km,
  circle_form = TRUE
) {
  na_result <- list(
    count_in_radius = 0,
    closest_distance_km = NA,
    mean_distance_km = NA,
    closest_point_lat = NA,
    closest_point_lon = NA,
    occ_data = if (is.null(occ_df)) {
      NULL
    } else {
      occ_df[0, , drop = FALSE]
    }
  )

  if (is.null(occ_df) || nrow(occ_df) == 0) {
    return(na_result)
  }

  occ_df <- occ_df |>
    filter(
      !is.na(.data$decimalLongitude),
      !is.na(.data$decimalLatitude)
    )
  if (nrow(occ_df) == 0) {
    return(na_result)
  }

  test_point <- sf::st_sfc(sf::st_point(c(longitude, latitude)), crs = 4326)
  occ_sf <- sf::st_as_sf(
    occ_df,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326
  )
  distances <- sf::st_distance(test_point, occ_sf)
  occ_df$distance_km <- as.numeric(distances) / 1000
  min_distance_km <- as.numeric(min(distances)) / 1000
  mean_distance_km <- mean(as.numeric(distances)) / 1000

  if (circle_form) {
    occ_df <- occ_df |>
      filter(.data$distance_km <= radius_km)
  }
  if (nrow(occ_df) == 0) {
    return(na_result)
  }

  list(
    count_in_radius = nrow(occ_df),
    closest_distance_km = round(min_distance_km, 2),
    mean_distance_km = round(mean_distance_km, 2),
    closest_point_lat = occ_df$decimalLatitude[which.min(occ_df$distance_km)],
    closest_point_lon = occ_df$decimalLongitude[which.min(occ_df$distance_km)],
    occ_data = occ_df
  )
}
