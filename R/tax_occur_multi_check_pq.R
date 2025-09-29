#' Check for taxa occurrences within a radius around multiple samples using GBIF data
#' @description
#' This function performs a species range check for taxa contained in a phyloseq
#' object, for multiple samples based on their geographic coordinates (longitude
#' and latitude).
#'
#' @param physeq (required) A phyloseq object.
#' @param taxonomic_rank The taxonomic rank to use for the check. Default is
#' "currentCanonicalSimple" which corresponds to the cleaned scientific names in
#' the phyloseq object if [gna_verifier_pq()] was used with default parameter.
#' @param min_occur Minimum number of occurrences in the radius to keep the taxon
#' (default: 0).
#' @param verbose (Logical, default: TRUE). Whether to print progress messages.
#' @param lon_column Column name in sample_data containing longitudes.
#' @param longitudes  Vector of longitudes corresponding to samples in the phyloseq object.
#'  If provided, it overrides lon_column.
#' @param lat_column Column name in sample_data containing latitudes.
#' @param latitudes Vector of latitudes corresponding to samples in the phyloseq
#' object. If provided, it overrides lat_column.
#' @param ... Additional parameters passed to [tax_occur_check()].
#'
#' @returns A list containing:
#'  - A tibble resulting from the concatenation of result of function
#'   [tax_occur_check()] for each GPS position.
#'  - A matrix of samples x taxa with the number of occurrences in the radius
#'    for each case of the matrix.
#'  - A new phyloseq object with taxa filtered based on min_occur. Be careful,
#'  the filtering may be very stringent.
#'
#' @export
#' @author Adrien Taudière
#'
#' @examples
#' res_occur_check <-
#'   tax_occur_multi_check_pq(subset_samples(data_fungi_mini_cleanNames, Diameter == 52),
#'     longitudes = c(8.31, 8.31, 8.64, -1.19, 7.03),
#'     latitudes = c(47.38, 47.38, 45.83, 43.65, 43.93)
#'   )
tax_occur_multi_check_pq <- function(
    physeq = NULL,
    taxonomic_rank = "currentCanonicalSimple",
    min_occur = 0,
    verbose = TRUE,
    lon_column = NULL,
    longitudes = NULL,
    lat_column = NULL,
    latitudes = NULL,
    ...) {
  if (is.null(longitudes) & !is.null(lon_column)) {
    longitudes <- as.numeric(sample_data(physeq)[, lon_column])
  } else if (is.null(longitudes) & is.null(lon_column)) {
    cli_error("Either {.arg longitudes} or {.arg lon_column} must be provided")
  } else if (!is.null(longitudes)) {
    if (length(longitudes) != nsamples(physeq)) {
      cli_error("The length of {.arg longitudes} must be equal to the number of samples in the phyloseq object")
    }
    physeq@sam_data <- sample_data(cbind(as.data.frame(physeq@sam_data), longitudes_for_multi_check = longitudes))
    lon_column <- "longitudes_for_multi_check"
  }

  if (is.null(latitudes) & !is.null(lat_column)) {
    latitudes <- as.numeric(sample_data(physeq)[, lat_column])
  } else if (is.null(latitudes) & is.null(lat_column)) {
    cli_error("Either {.arg latitudes} or {.arg lat_column} must be provided")
  } else if (!is.null(latitudes)) {
    if (length(latitudes) != nsamples(physeq)) {
      cli_error("The length of {.arg latitudes} must be equal to the number of samples in the phyloseq object")
    }
    physeq@sam_data <- sample_data(cbind(as.data.frame(physeq@sam_data), latitudes_for_multi_check = latitudes))
    lat_column <- "latitudes_for_multi_check"
  }

  longlat <- paste(longitudes, latitudes, sep = "_") |>
    unique()
  tax_range <- vector("list", length = length(longlat))
  names(tax_range) <- longlat
  
  # Initialize progress bar if verbose
  if (verbose) {
    pb <- cli_progress_bar(total = length(longlat))
  }
  
  for (i in seq_along(longlat)) {
    gps <- longlat[i]
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
      cli_message("Processing GPS point: {.val {gps}}")
    }
    long <- stringr::str_split_i(gps, "_", 1) |>
      as.numeric()
    lat <- stringr::str_split_i(gps, "_", 2) |>
      as.numeric()
    cond_sample <-
      sapply(as.vector(unlist(sample_data(physeq)[, lon_column])), function(x) {
        isTRUE(all.equal(x, long))
      }) &
        sapply(as.vector(unlist(sample_data(physeq)[, lat_column])), function(x) {
          isTRUE(all.equal(x, lat))
        })
    names(cond_sample) <- sample_names(physeq)
    new_physeq_i <- subset_samples_pq(physeq, cond_sample) |>
      clean_pq()
    tax_range[[gps]] <- tax_occur_check_pq(
      physeq = new_physeq_i,
      taxonomic_rank = taxonomic_rank,
      add_to_phyloseq = FALSE,
      verbose = verbose,
      longitude = long,
      latitude = lat,
      ...
    ) |>
      mutate(
        gps_point = gps, latitude = lat, longitude = long,
        sample_name = paste(sample_names(new_physeq_i), collapse = "___")
      )
  }
  
  # Complete progress bar
  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  taxtab_taxrank <- physeq@tax_table[, taxonomic_rank] |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "taxname") |>
    tibble()

  tax_range <- bind_rows(tax_range) |>
    tidyr::separate_rows(sample_name, sep = "___") |>
    relocate(sample_name)

  tax_range_mini <- tax_range |>
    filter(count_in_radius > min_occur) |>
    select(sample_name, taxa_name, count_in_radius) |>
    left_join(taxtab_taxrank,
      by = join_by("taxa_name" == !!sym(taxonomic_rank)), relationship = "many-to-many"
    )


  otu_matrix_occurence <- matrix(0, nrow = ntaxa(physeq), ncol = nsamples(physeq))

  for (i in 1:nrow(tax_range_mini)) {
    otu_matrix_occurence[
      which(taxa_names(physeq) == tax_range_mini$taxname[i]),
      which(sample_names(physeq) == tax_range_mini$sample_name[i])
    ] <- tax_range_mini$count_in_radius[i]
  }

  if (sum(otu_matrix_occurence > min_occur) != nrow(tax_range_mini)) {
    cli_error("Some taxa occurrences were not correctly mapped to the otu_matrix_occurence")
  }

  new_physeq <- taxa_as_rows(physeq)
  new_physeq@otu_table[otu_matrix_occurence == 0] <- 0

  new_physeq <- clean_pq(new_physeq, verbose = verbose)

  if (verbose) {
    remaining_taxa <- ntaxa(new_physeq)
    remaining_samples <- nsamples(new_physeq)
    remaining_occurrences <- sum(new_physeq@otu_table > 0)
    
    cli_message(c("After filtering taxa with at least {.val {min_occur + 1}} GBIF occurrences within {.val {radius_km}}km:",
                  "*" = "Taxa: {.val {remaining_taxa}}/{.val {ntaxa(physeq)}} remain",
                  "*" = "Samples: {.val {remaining_samples}}/{.val {nsamples(physeq)}} remain", 
                  "*" = "Occurrences: {.val {remaining_occurrences}}/{.val {sum(physeq@otu_table > 0)}} remain"))
  }
  return(list(
    "tax_range_list" = tax_range,
    "otu_matrix_occurence" = otu_matrix_occurence,
    "new_physeq" = new_physeq
  ))
}
