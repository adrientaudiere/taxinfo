#' Get altitude range statistics for each taxa from GBIF
#'
#' @description
#' Retrieve altitude/elevation statistics (minimum, maximum, 5%, 50%, 95% quantiles,
#' mean and standard deviation) for taxa from GBIF occurrence data.
#'
#' Two methods are available:
#' - **"gbif"** (default): Uses GBIF's native elevation field directly from occurrence
#'   records that have elevation data. This method doesn't require external packages
#'   but depends on the availability of elevation data in GBIF records.
#' - **"elevatr"**: Computes elevation from GPS coordinates using AWS Terrain Tiles
#'   via the `elevatr` package. This provides more complete coverage for occurrences
#'   that lack elevation data but requires the `elevatr` and `rnaturalearth` packages.
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
#'   - "gbif": Use GBIF's native elevation field (default). Only records with
#'     non-null elevation values are retrieved.
#'   - "elevatr": Compute elevation from GPS coordinates using AWS Terrain Tiles.
#'     Requires the `elevatr` and `rnaturalearth` packages.
#' @param n_occur (numeric, default 5000) Maximum number of occurrences to retrieve
#'   from GBIF for elevation computation.
#' @param elev_zoom (numeric, default 5) Zoom level for AWS Terrain Tiles.
#'   Only used when `method = "elevatr"`. Higher values give finer resolution
#'   but are slower. Range: 1-14. See [elevatr::get_elev_point()] for details.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param time_to_sleep (numeric, default 0.3) Time to sleep between two calls to
#'  rgbif::occ_search(). Useful to avoid being blocked by GBIF. Try to increase
#'  this value if you encounter rate limiting errors.
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#'  The returned data includes: altitude_min, altitude_max, altitude_q05, 
#'  altitude_q50, altitude_q95, altitude_mean, altitude_sd, altitude_n_records,
#'  and canonicalName. When `method = "elevatr"`, also includes altitude_n_ocean
#'  (number of points detected in ocean).
#' @export
#' @author Adrien Taudiere
#' @seealso [rgbif::occ_search()], [elevatr::get_elev_point()], [tax_gbif_occur_pq()], [plot_tax_gbif_pq()]
#' @details
#' ## Method "gbif" (default)
#' 
#' This method retrieves occurrence records from GBIF that have non-null elevation
#' values using `rgbif::occ_search()`. It filters records with `hasCoordinate=TRUE`,
#' `hasGeospatialIssue=FALSE`, and only returns records where the elevation field
#' is not null. This is the simplest approach and doesn't require additional packages.
#' 
#' Note: Many GBIF records lack elevation data, so the number of records with
#' elevation may be limited for some taxa.
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
#' ## GBIF Download API (Advanced)
#' 
#' For large-scale downloads, consider using `rgbif::occ_download()` directly with
#' `pred_notnull("elevation")` to filter records. This requires GBIF credentials:
#' - Register at <https://www.gbif.org/user/register>
#' - Store credentials in `.Renviron`: `GBIF_USER`, `GBIF_PWD`, `GBIF_EMAIL`
#' - See <https://docs.ropensci.org/rgbif/reference/occ_download.html> for details.
#' 
#' Please cite `rgbif` package. When using method "elevatr", also cite `elevatr`
#' and `rnaturalearth` packages.
#' 
#' @examples
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini)
#'
#' \donttest{
#' # Get altitude range statistics using GBIF elevation data (default)
#' data_fungi_mini_alt <- tax_gbif_alt(data_fungi_mini_cleanNames, 
#'                                      add_to_phyloseq = FALSE)
#'
#' # Using taxnames vector (returns a tibble)
#' tax_gbif_alt(taxnames = c("Amanita muscaria", "Boletus edulis"))
#'
#' # Use elevatr method to compute elevation from GPS coordinates
#' # (provides more coverage but requires elevatr and rnaturalearth packages)
#' altitude_elevatr <- tax_gbif_alt(
#'   taxnames = c("Amanita muscaria"),
#'   method = "elevatr",
#'   verbose = FALSE
#' )
#' 
#' # Add altitude data to phyloseq object
#' data_fungi_mini_with_alt <- tax_gbif_alt(data_fungi_mini_cleanNames)
#' 
#' # Visualize altitude ranges
#' library(ggplot2)
#' altitude_data <- tax_gbif_alt(
#'   taxnames = c("Amanita muscaria", "Boletus edulis", "Russula emetica"),
#'   verbose = FALSE
#' )
#' ggplot(altitude_data, aes(x = canonicalName)) +
#'   geom_pointrange(aes(y = altitude_mean, 
#'                       ymin = altitude_min, 
#'                       ymax = altitude_max)) +
#'   coord_flip() +
#'   labs(title = "Altitude Range by Species",
#'        x = "Species", y = "Altitude (m)")
#' }
tax_gbif_alt <- function(physeq = NULL,
                        taxnames = NULL,
                        taxonomic_rank = "currentCanonicalSimple",
                        add_to_phyloseq = NULL,
                        col_prefix = NULL,
                        method = c("gbif", "elevatr"),
                        n_occur = 5000,
                        elev_zoom = 5,
                        verbose = TRUE,
                        time_to_sleep = 0.3) {
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}, not both")
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }

  # Validate and set method

  method <- match.arg(method)

  # Check required packages for elevatr method
  if (method == "elevatr") {
    check_package("elevatr")
    check_package("rnaturalearth")
  }

  # Set default for add_to_phyloseq based on input type
  if (is.null(add_to_phyloseq)) {
    add_to_phyloseq <- !is.null(physeq)
  }

  if (!is.null(taxnames) && add_to_phyloseq) {
    cli::cli_abort("{.arg add_to_phyloseq} cannot be TRUE when {.arg taxnames} is provided")
  }

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  }

  gbif_taxa <- rgbif::name_backbone_checklist(taxnames) |>
    filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
    distinct()

  # Load world land boundaries for ocean detection (only needed for elevatr method)
  world_land <- NULL
  if (method == "elevatr") {
    world_land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }

  if (verbose) {
    pb <- cli::cli_progress_bar(total = length(gbif_taxa$usageKey))
    cli::cli_alert_info("Using method: {.val {method}}")
  }

  tib_occur_list <- vector("list", length(gbif_taxa$usageKey))
  for (i in seq_along(gbif_taxa$usageKey)) {
    x <- gbif_taxa$usageKey[i]
    Sys.sleep(time_to_sleep)
    species_name <- gbif_taxa$canonicalName[i]
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
      cli::cli_alert_info("Processing GBIF altitude data for {.emph {species_name}}")
    }
    
    elevation_data <- NULL
    n_ocean <- NA_integer_  # Only set for elevatr method
    
    if (method == "gbif") {
      # Method 1: Use GBIF's native elevation field
      # Retrieve records that have non-null elevation values
      occ_result <- rgbif::occ_search(
        x, 
        limit = n_occur, 
        fields = c("elevation"),
        hasCoordinate = TRUE,
        hasGeospatialIssue = FALSE
      )
      
      if (!is.null(occ_result$data) && nrow(occ_result$data) > 0) {
        elevation_data <- occ_result$data$elevation
        elevation_data <- elevation_data[!is.na(elevation_data)]
      }
      
    } else if (method == "elevatr") {
      # Method 2: Compute elevation from GPS coordinates using elevatr
      occ_result <- rgbif::occ_search(
        x, 
        limit = n_occur, 
        fields = c("decimalLatitude", "decimalLongitude"),
        hasCoordinate = TRUE,
        hasGeospatialIssue = FALSE
      )
      
      n_ocean <- 0
      
      if (!is.null(occ_result$data) && nrow(occ_result$data) > 0) {
        coords_df <- occ_result$data |>
          filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))
        
        if (nrow(coords_df) > 0) {
          # Convert to sf object for spatial operations
          coords_sf <- sf::st_as_sf(
            coords_df, 
            coords = c("decimalLongitude", "decimalLatitude"), 
            crs = 4326
          )
          
          # Detect ocean points (points NOT intersecting land)
          on_land <- lengths(sf::st_intersects(coords_sf, world_land)) > 0
          n_ocean <- sum(!on_land)
          
          # Warn about ocean points
          if (n_ocean > 0 && verbose) {
            cli::cli_alert_warning(
              "{.val {n_ocean}} occurrence(s) for {.emph {species_name}} are in the ocean and may have unreliable elevation"
            )
          }
          
          # Get elevation for all points using elevatr (AWS Terrain Tiles)
          if (verbose) {
            cli::cli_alert_info("Computing elevation from GPS coordinates using AWS Terrain Tiles...")
          }
          
          coords_with_elev <- tryCatch({
            suppressMessages(
              elevatr::get_elev_point(coords_sf, src = "aws", z = elev_zoom)
            )
          }, error = function(e) {
            if (verbose) {
              cli::cli_alert_warning(
                "Failed to retrieve elevation data for {.emph {species_name}}: {e$message}. You may want to try a different {.arg elev_zoom} level."
              )
            }
            return(NULL)
          })
          
          if (!is.null(coords_with_elev)) {
            elevation_data <- coords_with_elev$elevation
            elevation_data <- elevation_data[!is.na(elevation_data)]
          }
        }
      }
    }
    
    # Calculate statistics
    if (!is.null(elevation_data) && length(elevation_data) > 0) {
      tib <- tibble(
        "altitude_min" = min(elevation_data, na.rm = TRUE),
        "altitude_max" = max(elevation_data, na.rm = TRUE),
        "altitude_q05" = quantile(elevation_data, 0.05, na.rm = TRUE, names = FALSE),
        "altitude_q50" = quantile(elevation_data, 0.50, na.rm = TRUE, names = FALSE),
        "altitude_q95" = quantile(elevation_data, 0.95, na.rm = TRUE, names = FALSE),
        "altitude_mean" = mean(elevation_data, na.rm = TRUE),
        "altitude_sd" = sd(elevation_data, na.rm = TRUE),
        "altitude_n_records" = length(elevation_data),
        "canonicalName" = species_name
      )
      # Add ocean count only for elevatr method
      if (method == "elevatr") {
        tib$altitude_n_ocean <- n_ocean
      }
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
      # Add ocean count only for elevatr method
      if (method == "elevatr") {
        tib$altitude_n_ocean <- n_ocean
      }
    }
    tib_occur_list[[i]] <- tib
  }
  if (verbose) {
    cli::cli_progress_done(id = pb)
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
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")
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
