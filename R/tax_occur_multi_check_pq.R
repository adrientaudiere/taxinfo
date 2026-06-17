#' Check for taxa occurrences within a radius around multiple samples using GBIF data
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
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
#' @param radius_km Numeric. Search radius in kilometers (default: 50).
#'   See ?[tax_occur_check_pq()].
#' @param n_occur Numeric (default: 1000). Maximum number of occurrences to
#'  retrieve from GBIF for each taxon.
#' @param method (character, default `"download"`). How occurrences are fetched.
#'  `"download"` issues a **single** [rgbif::occ_download()] covering all taxa
#'  over the bounding box of every GPS point (**requires GBIF credentials**);
#'  `"search"` uses a per-taxon [rgbif::occ_search()] loop. See
#'  [tax_occur_check()].
#' @param circle_form (Logical, default: TRUE). Whether to use a circular search
#'  area. If FALSE, a square bounding box is used.
#' @param clean_coord (Logical, default: TRUE). Whether to clean coordinates
#'  using `CoordinateCleaner`.
#' @param clean_coord_verbose (Logical, default: FALSE). Whether to print
#'  messages from `CoordinateCleaner`.
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param ... Additional parameters (currently unused; reserved for forward
#'  compatibility).
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
#' @author Adrien Taudiere
#' @seealso [tax_occur_check()], [tax_occur_multi_pq()]
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini,
#'     data_sources = 210
#'   )
#' res_occur_check <-
#'   tax_occur_multi_check_pq(subset_samples(data_fungi_mini_cleanNames, Diameter == 52),
#'     longitudes = c(8.31, 8.31, 8.64, -1.19, 7.03),
#'     latitudes = c(47.38, 47.38, 45.83, 43.65, 43.93)
#'   )
#' }
tax_occur_multi_check_pq <- function(
  physeq = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  min_occur = 0,
  verbose = TRUE,
  lon_column = NULL,
  longitudes = NULL,
  lat_column = NULL,
  latitudes = NULL,
  radius_km = 50,
  n_occur = 1000,
  method = c("download", "search"),
  circle_form = TRUE,
  clean_coord = TRUE,
  clean_coord_verbose = FALSE,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE,
  ...
) {
  method <- match.arg(method)
  if (is.null(longitudes) & !is.null(lon_column)) {
    longitudes <- as.numeric(sample_data(physeq)[, lon_column])
  } else if (is.null(longitudes) & is.null(lon_column)) {
    cli::cli_abort(
      "Either {.arg longitudes} or {.arg lon_column} must be provided"
    )
  } else if (!is.null(longitudes)) {
    if (length(longitudes) != nsamples(physeq)) {
      cli::cli_abort(
        "The length of {.arg longitudes} must be equal to the number of samples in the phyloseq object"
      )
    }
    physeq@sam_data <- sample_data(cbind(
      as.data.frame(physeq@sam_data),
      longitudes_for_multi_check = longitudes
    ))
    lon_column <- "longitudes_for_multi_check"
  }

  if (is.null(latitudes) & !is.null(lat_column)) {
    latitudes <- as.numeric(sample_data(physeq)[, lat_column])
  } else if (is.null(latitudes) & is.null(lat_column)) {
    cli::cli_abort(
      "Either {.arg latitudes} or {.arg lat_column} must be provided"
    )
  } else if (!is.null(latitudes)) {
    if (length(latitudes) != nsamples(physeq)) {
      cli::cli_abort(
        "The length of {.arg latitudes} must be equal to the number of samples in the phyloseq object"
      )
    }
    physeq@sam_data <- sample_data(cbind(
      as.data.frame(physeq@sam_data),
      latitudes_for_multi_check = latitudes
    ))
    lat_column <- "latitudes_for_multi_check"
  }

  longlat <- paste(longitudes, latitudes, sep = "_") |>
    unique()
  tax_range <- vector("list", length = length(longlat))
  names(tax_range) <- longlat

  # Resolve every taxon in the phyloseq object once, then issue a SINGLE GBIF
  # download covering all taxa over the bounding box of all GPS points. Per-point
  # occurrence statistics are computed locally afterwards.
  all_taxnames <- taxonomic_rank_to_taxnames(
    physeq = physeq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = discard_genus_alone,
    discard_NA = discard_NA
  )
  gbif_taxa <- rgbif::name_backbone_checklist(all_taxnames) |>
    filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
    distinct()

  world_counts <- vapply(
    gbif_taxa$usageKey,
    function(k) {
      rgbif::occ_count(taxonKey = k, hasCoordinate = TRUE)
    },
    numeric(1)
  )

  bbox <- bbox_for_points(longitudes, latitudes, radius_km)
  occ_all <- fetch_occur_for_taxa(
    gbif_taxa = gbif_taxa,
    method = method,
    n_occur = n_occur,
    bbox = bbox,
    clean_coord = clean_coord,
    clean_coord_verbose = clean_coord_verbose,
    verbose = verbose
  )

  if (verbose) {
    pb <- cli::cli_progress_bar(total = length(longlat))
  }

  for (i in seq_along(longlat)) {
    gps <- longlat[i]
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
      cli::cli_alert_info("Processing GPS point: {.val {gps}}")
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

    # Restrict to the taxa present in this point's samples.
    taxnames_i <- taxonomic_rank_to_taxnames(
      physeq = new_physeq_i,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = discard_genus_alone,
      discard_NA = discard_NA
    )
    keep_i <- gbif_taxa$verbatim_name %in% taxnames_i
    gbif_taxa_i <- gbif_taxa[keep_i, , drop = FALSE]
    world_counts_i <- world_counts[keep_i]

    tax_range[[gps]] <- occur_check_compute_df(
      occ_all = occ_all,
      gbif_taxa = gbif_taxa_i,
      world_counts = world_counts_i,
      longitude = long,
      latitude = lat,
      radius_km = radius_km,
      circle_form = circle_form
    ) |>
      mutate(
        gps_point = gps,
        latitude = lat,
        longitude = long,
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
    left_join(
      taxtab_taxrank,
      by = join_by("taxa_name" == !!sym(taxonomic_rank)),
      relationship = "many-to-many"
    )

  otu_matrix_occurence <- matrix(
    0,
    nrow = ntaxa(physeq),
    ncol = nsamples(physeq)
  )

  for (i in 1:nrow(tax_range_mini)) {
    otu_matrix_occurence[
      which(taxa_names(physeq) == tax_range_mini$taxname[i]),
      which(sample_names(physeq) == tax_range_mini$sample_name[i])
    ] <- tax_range_mini$count_in_radius[i]
  }

  if (sum(otu_matrix_occurence > min_occur) != nrow(tax_range_mini)) {
    cli::cli_abort(
      "Some taxa occurrences were not correctly mapped to the otu_matrix_occurence"
    )
  }

  new_physeq <- taxa_as_rows(physeq)
  new_physeq@otu_table[otu_matrix_occurence == 0] <- 0

  new_physeq <- clean_pq(new_physeq, verbose = verbose)

  if (verbose) {
    remaining_taxa <- ntaxa(new_physeq)
    remaining_samples <- nsamples(new_physeq)
    remaining_occurrences <- sum(new_physeq@otu_table > 0)

    cli::cli_alert_info(c(
      "After filtering taxa with at least {.val {min_occur + 1}} GBIF occurrences within {.val {radius_km}}km:/n",
      "  - Taxa: {.val {remaining_taxa}}/{.val {ntaxa(physeq)}} remain/n",
      "  - Samples: {.val {remaining_samples}}/{.val {nsamples(physeq)}} remain/n",
      "  - Occurrences: {.val {remaining_occurrences}}/{.val {sum(physeq@otu_table > 0)}} remain"
    ))
  }
  return(list(
    "tax_range_list" = tax_range,
    "otu_matrix_occurence" = otu_matrix_occurence,
    "new_physeq" = new_physeq
  ))
}
