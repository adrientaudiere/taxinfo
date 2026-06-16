#' Get altitude range statistics for each taxa from GBIF
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Retrieve altitude/elevation statistics (minimum, maximum, 5%, 50%, 95% quantiles,
#' mean and standard deviation) for taxa from GBIF occurrence data.
#'
#' Two methods are available:
#' - **"gbif"** (default): Uses GBIF's Download API (`occ_download()`) to retrieve
#'   occurrence records with non-null elevation values. This is the recommended
#'   approach by GBIF for research purposes. **Requires GBIF credentials.**
#' - **"elevatr"**: Computes elevation from GPS coordinates retrieved from GBIF
#'   using AWS Terrain Tiles via the `elevatr` package.
#'   This provides more complete coverage for occurrences that lack elevation
#'   data but requires the `elevatr` and `rnaturalearth` packages.
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided)
#'  If TRUE, add new column(s) in the tax_table of the phyloseq object.
#'  Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#'  Cannot be TRUE if `taxnames` is provided.
#' @param col_prefix A character string to be added as a prefix to the new
#' columns names added to the tax_table slot of the phyloseq object (default: NULL).
#' @param method (character, default "gbif") Method to retrieve elevation data:
#'   - "gbif": Use GBIF's Download API with `pred_notnull("elevation")` to retrieve
#'     only records with elevation data. This is the recommended approach by GBIF
#'     for research. **Requires GBIF credentials** (see Details).
#'   - "elevatr": Compute elevation from GPS coordinates using AWS Terrain Tiles.
#'     Requires the `elevatr` and `rnaturalearth` packages.
#' @param elev_zoom (numeric, default 5) Zoom level for AWS Terrain Tiles.
#'   Only used when `method = "elevatr"`. Higher values give finer resolution
#'   but are slower. Range: 1-14. See [elevatr::get_elev_point()] for details.
#' @param n_coor_alt (int, default NULL) Number of occurrences to samples. If
#'   left to NULL, all occurrences are used to computed the altitute. It allow
#'   quicker computation when using method "elevatr" on taxa with a large
#'   number of occurrences.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#'  The returned data includes: altitude_min, altitude_max, altitude_q05,
#'  altitude_q50, altitude_q95, altitude_mean, altitude_sd, altitude_n_records,
#'  and canonicalName. When `method = "elevatr"`, also includes altitude_n_ocean
#'  (number of points detected in ocean).
#' @export
#' @author Adrien Taudiere
#' @seealso [rgbif::occ_download()], [elevatr::get_elev_point()], [tax_gbif_occur_pq()], [plot_tax_gbif_pq()]
#' @details
#' ## Method "gbif" (default)
#'
#' This method uses GBIF's Download API via `rgbif::occ_download()` with the
#' following predicates:
#' - `pred_in("taxonKey", gbif_taxon_keys)` - Filter by taxon keys
#' - `pred("hasCoordinate", TRUE)` - Only records with coordinates
#' - `pred("hasGeospatialIssue", FALSE)` - Exclude records with geospatial issues
#' - `pred_notnull("elevation")` - Only records with elevation data
#'
#' This is the recommended approach by GBIF for research purposes as it provides
#' citable downloads with DOIs.
#'
#' **GBIF credentials are required.** You must:
#' 1. Register at <https://www.gbif.org/user/register>
#' 2. Store credentials in your `.Renviron` file:
#'    - `GBIF_USER=your_username`
#'    - `GBIF_PWD=your_password`
#'    - `GBIF_EMAIL=your_email`
#' 3. See <https://docs.ropensci.org/rgbif/reference/occ_download.html> for more details.
#'
#' Note: Downloads are asynchronous and may take some time to complete.
#'
#' ## Method "elevatr"
#'
#' This method retrieves GPS coordinates from GBIF occurrence records and computes
#' elevation using AWS Terrain Tiles via the `elevatr` package. This provides more
#' complete coverage than relying on GBIF's elevation field.
#'
#' Ocean points are detected using land boundaries from `rnaturalearth` and
#' are reported in the `altitude_n_ocean` column. A warning is issued if
#' ocean points are detected for a taxon.
#'
#' Please cite `rgbif` package. When using method "elevatr", also cite `elevatr`
#' and `rnaturalearth` packages.
#'
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini)
#'
#' # Get altitude range statistics using GBIF Download API (default)
#' # Note: Requires GBIF credentials (GBIF_USER, GBIF_PWD, GBIF_EMAIL)
#' # Register at https://www.gbif.org/user/register
#' data_fungi_mini_alt <- tax_gbif_alt(data_fungi_mini_cleanNames,
#'   add_to_phyloseq = FALSE
#' )
#'
#' # Using taxnames vector (returns a tibble)
#' altitude_gbif <- tax_gbif_alt(
#'   taxnames = c("Amanita muscaria", "Boletus edulis")
#' )
#'
#' # Use elevatr method to compute elevation from GPS coordinates
#' # (provides more coverage, no GBIF credentials needed)
#' altitude_elevatr <- tax_gbif_alt(
#'   taxnames = c("Amanita muscaria"),
#'   method = "elevatr",
#'   n_coor_alt = 100,
#'   verbose = FALSE
#' )
#'
#' # Add altitude data to phyloseq object
#' data_fungi_mini_with_alt <- tax_gbif_alt(data_fungi_mini_cleanNames)
#'
#' data_fungi_mini_with_alt@tax_table |>
#'   as.data.frame() |>
#'   tibble() |>
#'   filter(as.numeric(altitude_n_records) > 100) |>
#'   distinct(taxa_name, .keep_all = TRUE) |>
#'   ggplot(aes(y = as.numeric(altitude_mean), x = taxa_name, fill = Guild)) +
#'   geom_col() +
#'   coord_flip() +
#'   geom_errorbar(
#'     aes(ymin = as.numeric(altitude_q05), ymax = as.numeric(altitude_q95)),
#'     width = 0.2
#'   ) +
#'   geom_label(aes(label = paste0("n=", altitude_n_records)), size = 2) +
#'   labs(
#'     title = "Mean altitude with 5%-95% quantiles (only taxa with >100 records)",
#'     subtitle = "Labels depict the number of gbif records with altitude data, \n
#'     color depict ecological Guild",
#'     x = "Taxa names",
#'     fill = "Guild"
#'   ) +
#'   theme(legend.position = "bottom")
#' }
tax_gbif_alt <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  method = c("gbif", "elevatr"),
  elev_zoom = 5,
  n_coor_alt = NULL,
  verbose = TRUE,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE
) {
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort(
      "You must specify either {.arg physeq} or {.arg taxnames}, not both"
    )
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }

  # Validate and set method
  method <- match.arg(method)

  # Check required packages for elevatr method
  if (method == "elevatr") {
    check_package("elevatr")
    check_package("terra")
    check_package("rnaturalearth")
  }

  # Set default for add_to_phyloseq based on input type
  if (is.null(add_to_phyloseq)) {
    add_to_phyloseq <- !is.null(physeq)
  }

  if (!is.null(taxnames) && add_to_phyloseq) {
    cli::cli_abort(
      "{.arg add_to_phyloseq} cannot be TRUE when {.arg taxnames} is provided"
    )
  }

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = discard_genus_alone,
      discard_NA = discard_NA
    )
  }

  gbif_taxa <- rgbif::name_backbone_checklist(taxnames) |>
    filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
    distinct()

  if (verbose) {
    cli::cli_alert_info("Using method: {.val {method}}")
  }

  tib_occur_list <- vector("list", nrow(gbif_taxa))

  gbif_taxon_keys <- gbif_taxa$usageKey

  if (verbose) {
    cli::cli_alert_info(
      "Submitting download request to GBIF for {.val {length(gbif_taxon_keys)}} taxa..."
    )
    cli::cli_alert_info(
      "Using predicates: pred_in('taxonKey'), pred('hasCoordinate', TRUE), pred('hasGeospatialIssue', FALSE)"
    )
  }

  if (method == "gbif") {
    # Method 1: Use GBIF Download API to get elevation data directly
    download_key <- tryCatch(
      {
        rgbif::occ_download(
          rgbif::pred_in("taxonKey", gbif_taxon_keys),
          rgbif::pred("hasCoordinate", TRUE),
          rgbif::pred("hasGeospatialIssue", FALSE),
          rgbif::pred_notnull("elevation"),
          format = "SIMPLE_CSV"
        )
      },
      error = function(e) {
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

    rgbif::occ_download_wait(download_key, quiet = !verbose)

    if (verbose) {
      cli::cli_alert_success("Download complete. Importing data...")
    }

    download_path <- rgbif::occ_download_get(download_key, overwrite = TRUE)
    occ_data <- rgbif::occ_download_import(download_path)
    file.remove(download_path)

    # Process elevation data for each taxon
    for (i in seq_len(nrow(gbif_taxa))) {
      taxon_key <- gbif_taxa$usageKey[i]
      species_name <- gbif_taxa$canonicalName[i]

      if (verbose) {
        cli::cli_alert_info(
          "Processing elevation data for {.emph {species_name}}"
        )
      }

      taxon_data <- occ_data |>
        filter(taxonKey == taxon_key)

      if (!is.null(n_coor_alt)) {
        taxon_data <-
          taxon_data |>
          slice_sample(n = n_coor_alt)
      }

      elevation_data <- NULL
      if (nrow(taxon_data) > 0) {
        elevation_data <- taxon_data$elevation
        elevation_data <- elevation_data[!is.na(elevation_data)]
      }

      # Calculate statistics
      if (!is.null(elevation_data) && length(elevation_data) > 0) {
        tib <- tibble(
          "altitude_min" = min(elevation_data, na.rm = TRUE),
          "altitude_max" = max(elevation_data, na.rm = TRUE),
          "altitude_q05" = quantile(
            elevation_data,
            0.05,
            na.rm = TRUE,
            names = FALSE
          ),
          "altitude_q50" = quantile(
            elevation_data,
            0.50,
            na.rm = TRUE,
            names = FALSE
          ),
          "altitude_q95" = quantile(
            elevation_data,
            0.95,
            na.rm = TRUE,
            names = FALSE
          ),
          "altitude_mean" = mean(elevation_data, na.rm = TRUE),
          "altitude_sd" = sd(elevation_data, na.rm = TRUE),
          "altitude_n_records" = length(elevation_data),
          "canonicalName" = species_name
        )
      } else {
        # No elevation data available
        tib <- tibble(
          "altitude_min" = NA_real_,
          "altitude_max" = NA_real_,
          "altitude_q05" = NA_real_,
          "altitude_q50" = NA_real_,
          "altitude_q95" = NA_real_,
          "altitude_mean" = NA_real_,
          "altitude_sd" = NA_real_,
          "altitude_n_records" = 0,
          "canonicalName" = species_name
        )
      }
      tib_occur_list[[i]] <- tib
    }
  } else if (method == "elevatr") {
    # Method 2: Compute elevation from GPS coordinates using elevatr
    # Use occ_download to get coordinates, then compute elevation

    # Load world land boundaries for ocean detection
    world_land <- NULL
    world_land <- rnaturalearth::ne_countries(
      scale = "medium",
      returnclass = "sf"
    )

    # Submit download request
    download_key <- tryCatch(
      {
        rgbif::occ_download(
          rgbif::pred_in("taxonKey", gbif_taxon_keys),
          rgbif::pred("hasCoordinate", TRUE),
          rgbif::pred("hasGeospatialIssue", FALSE),
          format = "SIMPLE_CSV"
        )
      },
      error = function(e) {
        cli::cli_abort(c(
          "Failed to submit GBIF download request.",
          "i" = "GBIF credentials are required. Please ensure you have set:",
          " " = "GBIF_USER, GBIF_PWD, GBIF_EMAIL in your .Renviron file",
          "i" = "Register at: {.url https://www.gbif.org/user/register}",
          "i" = "See: {.url https://docs.ropensci.org/rgbif/reference/occ_download.html}",
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

    # Wait for download to complete
    rgbif::occ_download_wait(download_key, quiet = !verbose)

    if (verbose) {
      cli::cli_alert_success("Download complete. Importing data...")
    }

    download_path <- rgbif::occ_download_get(download_key, overwrite = TRUE)
    occ_data <- rgbif::occ_download_import(download_path)
    file.remove(download_path)

    # Process elevation data for each taxon
    for (i in seq_len(nrow(gbif_taxa))) {
      taxon_key <- gbif_taxa$usageKey[i]
      species_name <- gbif_taxa$canonicalName[i]

      if (verbose) {
        cli::cli_alert_info(
          "Processing elevation data for {.emph {species_name}}"
        )
      }

      # Filter data for this taxon
      taxon_data <- occ_data |>
        filter(taxonKey == taxon_key)

      elevation_data <- NULL
      n_ocean <- 0

      if (nrow(taxon_data) > 0) {
        coords_df <- taxon_data |>
          filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))

        if (nrow(coords_df) > 0) {
          # Convert to sf object for spatial operations
          coords_sf <- sf::st_as_sf(
            coords_df,
            coords = c("decimalLongitude", "decimalLatitude"),
            crs = 4326
          )

          if (!is.null(n_coor_alt)) {
            coords_sf <-
              coords_sf |>
              slice_sample(n = n_coor_alt)
          }
          # Detect ocean points (points NOT intersecting land)
          on_land <- lengths(sf::st_intersects(coords_sf, world_land)) > 0
          n_ocean <- sum(!on_land)

          # Warn about ocean points
          if (n_ocean > 0 && verbose) {
            cli::cli_alert_warning(
              "{.val {n_ocean}} occurrence(s) (on a total of {.val {length(on_land)}}) for {.emph {species_name}} are in the ocean and may have unreliable elevation"
            )
          }

          # Get elevation for all points using elevatr (AWS Terrain Tiles)
          if (verbose) {
            cli::cli_alert_info(
              "Computing elevation from GPS coordinates using AWS Terrain Tiles..."
            )
          }

          coords_with_elev <- tryCatch(
            {
              suppressMessages(
                elevatr::get_elev_point(
                  coords_sf,
                  src = "aws",
                  z = elev_zoom,
                  overwrite = TRUE
                )
              )
            },
            error = function(e) {
              if (verbose) {
                cli::cli_alert_warning(
                  "Failed to retrieve elevation data for {.emph {species_name}}: {e$message}. You may want to try a different {.arg elev_zoom} level."
                )
              }
              return(NULL)
            }
          )

          if (!is.null(coords_with_elev)) {
            elevation_data <- coords_with_elev$elevation
            elevation_data <- elevation_data[!is.na(elevation_data)]
          }
        }
      }

      # Calculate statistics
      if (!is.null(elevation_data) && length(elevation_data) > 0) {
        tib <- tibble(
          "altitude_min" = min(elevation_data, na.rm = TRUE),
          "altitude_max" = max(elevation_data, na.rm = TRUE),
          "altitude_q05" = quantile(
            elevation_data,
            0.05,
            na.rm = TRUE,
            names = FALSE
          ),
          "altitude_q50" = quantile(
            elevation_data,
            0.50,
            na.rm = TRUE,
            names = FALSE
          ),
          "altitude_q95" = quantile(
            elevation_data,
            0.95,
            na.rm = TRUE,
            names = FALSE
          ),
          "altitude_mean" = mean(elevation_data, na.rm = TRUE),
          "altitude_sd" = sd(elevation_data, na.rm = TRUE),
          "altitude_n_records" = length(elevation_data),
          "altitude_n_ocean" = n_ocean,
          "canonicalName" = species_name
        )
      } else {
        # No elevation data available
        tib <- tibble(
          "altitude_min" = NA_real_,
          "altitude_max" = NA_real_,
          "altitude_q05" = NA_real_,
          "altitude_q50" = NA_real_,
          "altitude_q95" = NA_real_,
          "altitude_mean" = NA_real_,
          "altitude_sd" = NA_real_,
          "altitude_n_records" = 0,
          "altitude_n_ocean" = n_ocean,
          "canonicalName" = species_name
        )
      }
      tib_occur_list[[i]] <- tib
    }
  }

  tib_occur <- bind_rows(tib_occur_list)

  # Get new column names (excluding canonicalName which is used for join)
  new_cols <- setdiff(colnames(tib_occur), "canonicalName")

  # Check for column name collisions and handle col_prefix
  if (add_to_phyloseq) {
    existing_cols <- colnames(physeq@tax_table)
    common_cols <- intersect(paste0(col_prefix, new_cols), existing_cols)

    if (length(common_cols) > 0 && is.null(col_prefix)) {
      cli::cli_warn(c(
        "Column names already exist in tax_table: {.val {common_cols}}",
        "i" = "Adding prefix 'gbif_alt_' to avoid conflicts"
      ))
      col_prefix <- "gbif_alt_"
    }
  }

  # Apply col_prefix to new columns
  if (!is.null(col_prefix)) {
    tib_occur <- tib_occur |>
      rename_with(~ paste0(col_prefix, .), .cols = -canonicalName)
  }

  if (add_to_phyloseq) {
    new_physeq <- physeq
    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(
      unclass(new_physeq@tax_table[, taxonomic_rank]),
      1,
      paste0,
      collapse = " "
    )
    new_physeq@tax_table <-
      left_join(tax_tab, tib_occur, by = join_by(taxa_name == canonicalName)) |>
      as.matrix() |>
      tax_table()

    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    return(tib_occur)
  }
}
