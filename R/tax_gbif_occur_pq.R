#' Get number of occurrences for each taxa of a phyloseq object
#'
#' @description
#' A wrapper of [rgbif::occ_search()] function to get the number of occurences.
#' Optionally, the number of occurrences can be obtained by years or by country.
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
#' @param by_country (logical, default FALSE) If TRUE, the number of occurences
#'   is computed by country
#' @param by_years (logical, default FALSE) If TRUE, the number of occurences
#'   is computed by years
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param time_to_sleep (numeric, default 0.3) Time to sleep between two calls to
#'  rgbif::occ_search(). Useful to avoid to be blocked by GBIF. Try to increase
#'  this value if you are blocked by the error "To download GBIF occurrence data in bulk, please request..."
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#' @export
#' @author Adrien Taudière
#' @details
#' This function is mainly a wrapper of the work of others.
#'  Please cite `rgbif` package.
#' @examples
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini)
#' data_fungi_mini_cleanNames <- tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_country = TRUE)
#'
#' # Get data without adding to phyloseq
#' tax_gbif_occur_pq(data_fungi_mini_cleanNames, add_to_phyloseq = FALSE)
#' tax_gbif_occur_pq(data_fungi_mini_cleanNames, by_years = TRUE, add_to_phyloseq = FALSE)
#'
#' # Using taxnames vector (returns a tibble)
#' tax_gbif_occur_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
#' ggplot(
#'   data_fungi_mini_cleanNames@tax_table,
#'   aes(y = log10(as.numeric(Global_occurences)), x = currentCanonicalSimple)
#' ) +
#'   geom_col() +
#'   geom_col(aes(y = -log10(as.numeric(FR))), fill = "blue") +
#'   coord_flip() +
#'   xlab("Number of occurences (log10 scale) at global (grey) scale and in France (blue)")
tax_gbif_occur_pq <- function(physeq = NULL,
                              taxnames = NULL,
                              taxonomic_rank = "currentCanonicalSimple",
                              add_to_phyloseq = NULL,
                              col_prefix = NULL,
                              by_country = FALSE,
                              by_years = FALSE,
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
    select(-verbatim_index) |> # in order to duplicate
    distinct()

  if (by_country && by_years) {
    cli::cli_abort("You can't set both {.arg by_country} and {.arg by_years} to TRUE")
  } else if (by_country) {
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
        cli::cli_alert_info("Processing GBIF occurrences for {.emph {species_name}}")
      }
      tib <- rgbif::occ_search(x, limit = 0, facet = "country")$facet$country
      tib$canonicalName <- gbif_taxa$canonicalName[which(gbif_taxa$usageKey == x)]
      tib_occur_list[[i]] <- tib
    }
    if (verbose) {
      cli::cli_progress_done(id = pb)
    }
    tib_occur <- bind_rows(tib_occur_list)
  } else if (by_years) {
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
        cli::cli_alert_info("Processing GBIF occurrences for {.emph {species_name}}")
      }
      tib <- rgbif::occ_search(x, limit = 0, facet = "year")$facet$year
      tib$canonicalName <- gbif_taxa$canonicalName[which(gbif_taxa$usageKey == x)]
      tib_occur_list[[i]] <- tib
    }
    if (verbose) {
      cli::cli_progress_done(id = pb)
    }
    tib_occur <- bind_rows(tib_occur_list)
  } else {
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
        cli::cli_alert_info("Processing GBIF occurrences for {.emph {species_name}}")
      }
      tib <- tibble(
        "Global_occurences" = rgbif::occ_search(x, limit = 0)$meta$count,
        "canonicalName" = gbif_taxa$canonicalName[which(gbif_taxa$usageKey == x)]
      )
      tib_occur_list[[i]] <- tib
    }
    if (verbose) {
      cli::cli_progress_done(id = pb)
    }
    tib_occur <- bind_rows(tib_occur_list)
  }

  if (by_country | by_years) {
    tib_occur <- tib_occur |>
      group_by(canonicalName) |>
      tidyr::pivot_wider(
        names_from = name,
        values_from = count
      )
  }

  # Get new column names (excluding canonicalName which is used for join)
  new_cols <- setdiff(colnames(tib_occur), "canonicalName")

  # Check for column name collisions and handle col_prefix
  if (add_to_phyloseq) {
    existing_cols <- colnames(physeq@tax_table)
    common_cols <- intersect(paste0(col_prefix, new_cols), existing_cols)

    if (length(common_cols) > 0 && is.null(col_prefix)) {
      cli::cli_warn(c(
        "Column names already exist in tax_table: {.val {common_cols}}",
        "i" = "Adding prefix 'gbif_' to avoid conflicts"
      ))
      col_prefix <- "gbif_"
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
