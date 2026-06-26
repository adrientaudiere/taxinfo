#' Get number of occurrences for each taxa of a phyloseq object
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
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
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#' @export
#' @author Adrien Taudiere
#' @seealso [rgbif::occ_search()], [plot_tax_gbif_pq()], [tax_occurr_pq()]
#' @details
#' This function is mainly a wrapper of the work of others.
#'  Please cite `rgbif` package.
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini)
#'
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
#' }
tax_gbif_occur_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  by_country = FALSE,
  by_years = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE
) {
  resolved <- resolve_taxa_input(
    physeq = physeq,
    taxnames = taxnames,
    add_to_phyloseq = add_to_phyloseq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = discard_genus_alone,
    discard_NA = discard_NA
  )
  taxnames <- resolved$taxnames
  add_to_phyloseq <- resolved$add_to_phyloseq

  gbif_taxa <- rgbif::name_backbone_checklist(taxnames) |>
    filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
    distinct()

  if (by_country && by_years) {
    cli::cli_abort(
      "You can't set both {.arg by_country} and {.arg by_years} to TRUE"
    )
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
        cli::cli_alert_info(
          "Processing GBIF occurrences for {.emph {species_name}}"
        )
      }
      tib <- rgbif::occ_search(x, limit = 0, facet = "country")$facet$country
      species_query <- gbif_taxa$verbatim_name[which(
        gbif_taxa$usageKey == x
      )]
      if (is.null(tib) || nrow(tib) == 0) {
        tib <- tibble(
          name = character(0),
          count = integer(0),
          query_name = character(0)
        )
      } else {
        tib$query_name <- species_query
      }
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
        cli::cli_alert_info(
          "Processing GBIF occurrences for {.emph {species_name}}"
        )
      }
      tib <- rgbif::occ_search(x, limit = 0, facet = "year")$facet$year
      species_query <- gbif_taxa$verbatim_name[which(
        gbif_taxa$usageKey == x
      )]
      if (is.null(tib) || nrow(tib) == 0) {
        tib <- tibble(
          name = character(0),
          count = integer(0),
          query_name = character(0)
        )
      } else {
        tib$query_name <- species_query
      }
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
        cli::cli_alert_info(
          "Processing GBIF occurrences for {.emph {species_name}}"
        )
      }
      tib <- tibble(
        "Global_occurences" = rgbif::occ_search(x, limit = 0)$meta$count,
        "query_name" = gbif_taxa$verbatim_name[which(
          gbif_taxa$usageKey == x
        )]
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
      group_by(query_name) |>
      tidyr::pivot_wider(
        names_from = name,
        values_from = count
      )
  }

  if (add_to_phyloseq) {
    return(augment_tax_table(
      physeq,
      tib_occur,
      taxonomic_rank = taxonomic_rank,
      info_key = "query_name",
      col_prefix = col_prefix,
      default_prefix = "gbif_"
    ))
  } else {
    return(tib_occur)
  }
}
