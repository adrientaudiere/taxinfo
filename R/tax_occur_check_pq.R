#' Check for taxa occurrences within a radius around samples using GBIF data
#'
#' @description
#' This function performs a species range check for taxa contained
#'   in a phyloseq object. The result can optionally be added to the phyloseq
#'   object's tax_table as new columns.
#'
#' @param physeq (optional) phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#'  The phyloseq object containing the taxa to check.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank Character. The taxonomic rank to use for the check.
#'  Default is "currentCanonicalSimple" which corresponds to the cleaned
#'  scientific names in the phyloseq object if [gna_verifier_pq()] was used with
#'  default parameter.
#' @param longitude Numeric. Longitude of the test point in decimal degrees.
#' @param latitude Numeric. Latitude of the test point in decimal degrees.
#' @param radius_km Numeric. Search radius in kilometers (default: 50).
#' @param n_occur Numeric. Maximum number of occurrences to retrieve from GBIF
#'  for each taxon (default: 1000).
#' @param add_to_phyloseq  (Logical, default TRUE when physeq is provided, FALSE when taxnames is provided).
#'  Whether to add the results as new columns in the phyloseq object's tax_table. If TRUE, the results will be
#'  appended to the tax_table with appropriate column names.
#'  Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#'  Cannot be TRUE if `taxnames` is provided.
#' @param col_prefix A character string to be added as a prefix to the new
#' columns names added to the tax_table slot of the phyloseq object (default: NULL).
#' @param verbose (Logical, default: TRUE). Whether to print progress messages.
#' @param ... Additional parameters passed to [tax_occur_check()].
#'
#' @return Either a data frame (if add_to_phyloseq = FALSE) or a new phyloseq
#' object (if add_to_phyloseq = TRUE).
#'
#' @author Adrien Taudière
#'
#' @examples
#' 
#' @seealso [tax_occur_check()], [tax_occur_multi_check_pq()], [tax_check_ecoregion()]
#'
#' check_res <- tax_occur_check_pq(data_fungi_mini_cleanNames,
#'   longitude = 2.3,
#'   latitude = 48,
#'   radius_km = 100,
#'   n_occur = 50,
#'   add_to_phyloseq = FALSE
#' )
#'
#' check_res |>
#'   mutate(taxa_name = forcats::fct_reorder(taxa_name, count_in_radius)) |>
#'   ggplot(aes(x = count_in_radius, y = taxa_name, fill = total_count_in_world)) +
#'   geom_col()
#'
#' data_fungi_mini_cleanNames_range_verif <-
#'   tax_occur_check_pq(data_fungi_mini_cleanNames,
#'     longitude = 2.3,
#'     latitude = 48,
#'     radius_km = 50,
#'     n_occur = 10
#'   )
#'
#' df <- data_fungi_mini_cleanNames_range_verif@tax_table[, "count_in_radius"] |>
#'   table(useNA = "always") |>
#'   data.frame()
#'
#' colnames(df) <- c("count_in_radius", "n_taxa")
#' df
#'
#' # Subset taxa with at least one occurrence in the radius
#' cond_count_sup_0 <-
#'   data_fungi_mini_cleanNames_range_verif@tax_table[, "count_in_radius"] |>
#'     as.numeric() > 0
#' cond_count_sup_0[is.na(cond_count_sup_0)] <- FALSE
#' names(cond_count_sup_0) <- taxa_names(data_fungi_mini_cleanNames_range_verif)
#'
#' subset_taxa_pq(data_fungi_mini_cleanNames_range_verif, cond_count_sup_0) |>
#'   summary_plot_pq()
#' @export
tax_occur_check_pq <- function(physeq = NULL,
                               taxnames = NULL,
                               taxonomic_rank = "currentCanonicalSimple",
                               longitude = NULL,
                               latitude = NULL,
                               radius_km = 50,
                               n_occur = 1000,
                               add_to_phyloseq = NULL,
                               col_prefix = NULL,
                               verbose = TRUE,
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

  if (is.null(taxnames)) {
    taxnames_raw <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  } else {
    taxnames_raw <- taxnames
  }

  if (length(taxnames_raw) == 0) {
    if (verbose) {
      cli::cli_alert_warning(c("No taxonomic names found at the specified taxonomic rank.",
        "i" = "Please check the {.arg taxonomic_rank} parameter and your phyloseq object."
      ))
    }
    if (add_to_phyloseq) {
      return(physeq)
    } else {
      return(data.frame(
        "taxa_name" = NA,
        "count_in_radius" = NA,
        "closest_distance_km" = NA,
        "mean_distance_km" = NA,
        "total_count_in_world" = NA,
        "search_radius" = NA,
        "closest_point_lat" = NA,
        "closest_point_lon" = NA,
        "sample_point_lat" = NA,
        "sample_point_lon" = NA
      ))
    }
  }
  gbif_taxa <- rgbif::name_backbone_checklist(taxnames_raw) |>
    filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
    distinct()
  taxnames <- gbif_taxa$canonicalName

  tax_range <- lapply(taxnames, function(name) {
    tax_occur_check(name,
      verbose = verbose,
      longitude = longitude,
      latitude = latitude,
      n_occur = n_occur,
      radius_km = radius_km, ...
    )
  }) |>
    setNames(paste(taxnames)) |>
    list2DF() |>
    t()

  tax_range <- tax_range |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "taxa_name") |>
    tibble() |>
    tidyr::unnest(cols = everything())

  colnames(tax_range) <- c(
    "taxa_name",
    "count_in_radius",
    "closest_distance_km",
    "mean_distance_km",
    "total_count_in_world",
    "search_radius",
    "closest_point_lat",
    "closest_point_lon",
    "sample_point_lat",
    "sample_point_lon"
  )

  # Determine new column names (excluding taxa_name which is used for join)
  new_cols <- c(
    "count_in_radius", "closest_distance_km", "mean_distance_km",
    "total_count_in_world", "search_radius", "closest_point_lat",
    "closest_point_lon", "sample_point_lat", "sample_point_lon"
  )

  # Check for column name collisions and handle col_prefix
  if (add_to_phyloseq) {
    existing_cols <- colnames(physeq@tax_table)
    common_cols <- intersect(paste0(col_prefix, new_cols), existing_cols)

    if (length(common_cols) > 0 && is.null(col_prefix)) {
      cli::cli_warn(c(
        "Column names already exist in tax_table: {.val {common_cols}}",
        "i" = "Adding prefix 'occur_' to avoid conflicts"
      ))
      col_prefix <- "occur_"
    }
  }

  # Apply col_prefix to new columns
  if (!is.null(col_prefix)) {
    tax_range <- tax_range |>
      rename_with(~ paste0(col_prefix, .), .cols = -taxa_name)
  }

  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")

    new_physeq@tax_table <-
      left_join(tax_tab, tax_range, by = join_by(taxa_name)) |>
      as.matrix() |>
      tax_table()

    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    return(tax_range)
  }
}
