#' Get biotic interactions for taxa present in a phyloseq object using rglobi
#'
#' @description
#'  A wrapper of [rglobi::get_interactions_by_taxa()] function to get biotic
#'  interactions for each taxa of a phyloseq object
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param taxnames (optional) A character vector of taxonomic names. If provided, `physeq` is ignored.
#' @param discard_synonym (logical, default TRUE) If TRUE, discard interactions
#'   where the source_taxon_name is a synonym of the taxon name used to query
#' @param add_to_phyloseq (logical, default FALSE) If TRUE, return a new phyloseq
#' object with new columns in the tax_table slot. If FALSE, return a tibble
#' with the interactions found for each taxon. Cannot be TRUE if `taxnames` is provided.
#' @param interaction_types A character vector of interaction types to
#'   query. See [rglobi::get_interaction_types()]. If NULL (default),
#'   all interaction types are queried.
#' @param valid_taxo_target_taxon (logical, default TRUE) If TRUE, verify the
#'  scientific names of the target_taxon_name using [taxize::gna_verifier()]
#'  function and keep only valid names.
#' @param add_target_canonical (logical, default TRUE) If TRUE, add a column
#'   `target_taxon_Canonical` with the current accepted name (resolve the
#'   synonymie) of the target_taxon_name using [taxize::gna_verifier()] function.
#' @param data_sources	A character or integer vector with numbers corresponding
#'  to data sources. See the Global Names Architecture documentation for a list
#'  of available options.
#' @param verbose (logical, default FALSE) If TRUE, prompt some messages.
#' @param strict_interaction_types (logical, default TRUE) If TRUE, keep only
#'  interactions exactly matching the interaction_types provided. If FALSE, keep
#'  all interactions returned by rglobi for the queried taxon. For exemple,
#'  rglobi for interaction_types = "hasHost" will also return interactions
#'   with interaction_type = "pathogenOf" and "parasiteOf" if
#'   strict_interaction_types is set to FALSE.
#' @param max_interactions (numeric, default 1000) The maximum number of interactions
#'  to query for each taxon.
#' @param batch_size_gna_verifier (numeric, default 100) The number of names to
#'  verify at once with' [taxize::gna_verifier()] function. Its a hack because
#'  gna_verifier seems to fail when too many names are sent at once including
#'  strange ones such as what is obtain whith rglobi. Only used if
#'   `valid_taxo_target_taxon` is set to TRUE.
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#' object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#' @author Adrien Taudière
#' @export
#'
#' @importFrom rglobi get_interactions_by_taxa
#'
#' @examples
#' res_globi <- tax_globi_pq(data_fungi_mini,
#'   taxonomic_rank = c("Genus", "Species"),
#'   interaction_types = list("parasiteOf", "hasHost"),
#'   verbose = TRUE,
#'   max_interactions = 10
#' )
#'
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini,
#'   data_sources = 210,
#'   add_to_phyloseq = TRUE
#' )
#'
#' data_fungi_mini_cleanNames <- tax_globi_pq(data_fungi_mini_cleanNames,
#'   interaction_types = c("hasHost"),
#'   data_sources = 210,
#'   add_to_phyloseq = TRUE
#' )
#' @details
#'  This function is mainly a wrapper of the work of others.
#'  Please cite `rglobi` and `taxize` packages.
tax_globi_pq <- function(physeq = NULL,
                         taxonomic_rank = "currentCanonicalSimple",
                         taxnames = NULL,
                         discard_synonym = TRUE,
                         add_to_phyloseq = FALSE,
                         interaction_types = NULL,
                         valid_taxo_target_taxon = TRUE,
                         add_target_canonical = TRUE,
                         data_sources = c(1, 12),
                         verbose = FALSE,
                         strict_interaction_types = TRUE,
                         max_interactions = 1000,
                         batch_size_gna_verifier = 50) {
  check_package("rglobi")

  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}, not both")
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }
  if (!is.null(taxnames) && add_to_phyloseq) {
    cli::cli_abort("{.arg add_to_phyloseq} cannot be TRUE when {.arg taxnames} is provided")
  }

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE,
      discard_NA = TRUE
    )
  }

  tib_globi_all <- NULL

  for (tax_i in taxnames) {
    tib_globi <- rglobi::get_interactions_by_taxa(
      interactiontype = interaction_types,
      sourcetaxon = tax_i,
      showfield = c("source_taxon_name", "interaction_type", "target_taxon_name"),
      otherkeys = list(limit = max_interactions)
    ) |>
      group_by(across(everything())) |>
      summarise(nb = n(), .groups = "keep")

    if (strict_interaction_types & !is.null(interaction_types)) {
      tib_globi <- tib_globi |>
        filter(interaction_type %in% interaction_types)
    }

    if (discard_synonym) {
      tib_globi <- tib_globi |>
        filter(source_taxon_name == tax_i)
    }

    if (nrow(tib_globi) == 0) {
      if (verbose) {
        cli::cli_alert_warning("No interaction found for {.emph {tax_i}}")
      }
    } else {
      tib_globi <- tib_globi |>
        group_by(interaction_type, target_taxon_name) |>
        summarise(nb = sum(nb), .groups = "keep") |>
        arrange(desc(nb))

      if (valid_taxo_target_taxon) {
        nb_int_before <- nrow(tib_globi)
        # extract only letters and space to avoid issues with gna_verifier
        target_taxon_name <- stringr::str_extract_all(
          pattern = "[A-Za-z ]+",
          unique(tib_globi$target_taxon_name),
          simplify = TRUE
        )[, 1]

        if (length(target_taxon_name) > batch_size_gna_verifier) {
          n_names <- length(target_taxon_name)
          n_batches <- ceiling(n_names / batch_size_gna_verifier)

          verif_target_taxon_list <- list()

          for (i in 1:n_batches) {
            start_idx <- (i - 1) * batch_size_gna_verifier + 1
            end_idx <- min(i * batch_size_gna_verifier, n_names)
            current_batch <- target_taxon_name[start_idx:end_idx]

            batch_result <- taxize::gna_verifier(current_batch, data_sources = data_sources)
            verif_target_taxon_list[[i]] <- batch_result
          }
          verif_target_taxon <- do.call(rbind, verif_target_taxon_list)
        } else {
          verif_target_taxon <- taxize::gna_verifier(target_taxon_name, data_sources = data_sources)
        }


        tib_globi <- tib_globi |>
          filter(target_taxon_name %in% verif_target_taxon$submittedName[!is.na(verif_target_taxon$currentCanonicalSimple)])

        tib_globi <- tib_globi |>
          right_join(distinct(data.frame("target_taxon_name" = verif_target_taxon$submittedName, "target_taxon_Canonical" = verif_target_taxon$currentCanonicalSimple)),
            by = join_by(target_taxon_name)
          ) |>
          filter(!is.na(interaction_type))

        tib_globi_i <- tib_globi |>
          tidyr::pivot_wider(
            names_from = interaction_type,
            values_from = target_taxon_Canonical
          ) |>
          ungroup() |>
          mutate(across(everything(), function(x) {
            paste0(na.omit(x), collapse = "; ")
          })) |>
          distinct()

        if (!add_target_canonical) {
          tib_globi_i <- tib_globi_i |>
            select(-target_taxon_Canonical)
        }

        if (verbose) {
          cli::cli_alert_info("After verification of valid target taxon names: {.val {nrow(tib_globi)}}/{.val {nb_int_before}} interactions kept for {.emph {tax_i}}")
        }
      } else {
        tib_globi_i <- tib_globi |>
          tidyr::pivot_wider(names_from = interaction_type, values_from = target_taxon_name) |>
          mutate(across(everything(), function(x) {
            paste0(na.omit(x), collapse = "; ")
          })) |>
          distinct()
      }

      tib_globi_i$taxa_name <- tax_i
      tib_globi_all <- bind_rows(
        tib_globi_all,
        tib_globi_i
      )
    }
  }
  if (is.null(tib_globi_all)) {
    if (verbose) {
      cli::cli_alert_warning(c("No interaction found for any taxon at the specified taxonomic rank.",
        "i" = "Please check the {.arg taxonomic_rank} parameter and your phyloseq object."
      ))
    }
    if (add_to_phyloseq) {
      return(physeq)
    } else {
      return(tib_globi_all)
    }
  }
  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")

    new_physeq@tax_table <-
      left_join(tax_tab, tib_globi_all, by = join_by(taxa_name)) |>
      as.matrix() |>
      tax_table()

    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    tib_globi_all <- tib_globi_all |>
      relocate(taxa_name)
    return(tib_globi_all)
  }
}
