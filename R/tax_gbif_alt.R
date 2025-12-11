#' Get altitude range statistics for each taxa from GBIF
#'
#' @description
#' A wrapper of [rgbif::occ_search()] function to retrieve altitude/elevation
#' statistics (minimum, maximum, 5%, 50%, 95% quantiles, mean and standard deviation)
#' from GBIF occurrence data with elevation information.
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
#' @param n_occur_altitude (numeric, default 5000) Maximum number of occurrences WITH
#'   altitude data to retrieve from GBIF. The function will retrieve enough records to get
#'   this many records with elevation data (excluding NA values).
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param time_to_sleep (numeric, default 0.3) Time to sleep between two calls to
#'  rgbif::occ_search(). Useful to avoid to be blocked by GBIF. Try to increase
#'  this value if you are blocked by the error "To download GBIF occurrence data in bulk, please request..."
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#'  The returned data includes: altitude_min, altitude_max, altitude_q05, 
#'  altitude_q50, altitude_q95, altitude_mean, altitude_sd, altitude_n_records,
#'  and canonicalName.
#' @export
#' @author Adrien Taudiere
#' @seealso [rgbif::occ_search()], [tax_gbif_occur_pq()], [plot_tax_gbif_pq()]
#' @details
#' This function is mainly a wrapper of the work of others.
#'  Please cite `rgbif` package.
#'  
#' The function retrieves occurrence records with elevation data from GBIF and
#' calculates altitude statistics. The `n_occur_altitude` parameter specifies
#' the target number of records WITH elevation data (non-NA values). The function
#' may retrieve more total records to reach this target.
#' 
#' @examples
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini)
#'
#' \donttest{
#' # Get altitude range statistics
#' data_fungi_mini_alt <- tax_gbif_alt(data_fungi_mini_cleanNames, 
#'                                      add_to_phyloseq = FALSE)
#'
#' # Using taxnames vector (returns a tibble)
#' tax_gbif_alt(taxnames = c("Amanita muscaria", "Boletus edulis"))
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
                        n_occur_altitude = 5000,
                        verbose = TRUE,
                        time_to_sleep = 0.3) {
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

  if (verbose) {
    pb <- cli::cli_progress_bar(total = length(gbif_taxa$usageKey))
  }

  tib_occur_list <- vector("list", length(gbif_taxa$usageKey))
  for (i in seq_along(gbif_taxa$usageKey)) {
    x <- gbif_taxa$usageKey[i]
    Sys.sleep(time_to_sleep)
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
      species_name <- gbif_taxa$canonicalName[which(gbif_taxa$usageKey == x)]
      cli::cli_alert_info("Processing GBIF altitude data for {.emph {species_name}}")
    }
    
    # Retrieve occurrences with elevation data
    # We retrieve more records initially to get enough with elevation data
    # Start with a multiplier to account for records without elevation
    max_limit <- min(n_occur_altitude * 3, 100000)  # Cap at 100k for safety
    
    occ_result <- rgbif::occ_search(
      x, 
      limit = max_limit, 
      fields = c("elevation"),
      hasCoordinate = TRUE
    )
    
    # Extract elevation data and filter NA values
    elevation_data <- NULL
    if (!is.null(occ_result$data) && nrow(occ_result$data) > 0) {
      elevation_data <- occ_result$data$elevation
      elevation_data <- elevation_data[!is.na(elevation_data)]
      
      # If we have more than requested, sample down to n_occur_altitude
      if (length(elevation_data) > n_occur_altitude) {
        elevation_data <- elevation_data[1:n_occur_altitude]
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
        "canonicalName" = gbif_taxa$canonicalName[which(gbif_taxa$usageKey == x)]
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
        "canonicalName" = gbif_taxa$canonicalName[which(gbif_taxa$usageKey == x)]
      )
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
