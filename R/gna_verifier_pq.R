#' Verify (and fix) scientific names (Genus species) of a phyloseq object.
#'
#' @description
#'  A wrapper of [taxize::gna_verifier] apply to phyloseq object
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character)
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. the default c("Genus", "Species")).
#' @param data_sources A character or integer vector.
#'   See [taxize::gna_verifier] documentation. For example,
#'   1=Catalogue of Life, 3=ITIS, 5=Index Fungarum, 11=GBIF backbone and
#'   210=TaxRef.
#' @param all_matches (Logical) See [taxize::gna_verifier] documentation.
#' @param capitalize (Logical) See [taxize::gna_verifier] documentation.
#' @param species_group (Logical) See [taxize::gna_verifier] documentation.
#' @param fuzzy_uninomial (Logical) See [taxize::gna_verifier] documentation.
#' @param stats (Logical) See [taxize::gna_verifier] documentation.
#' @param main_taxon_threshold (numeric) See [taxize::gna_verifier]
#'   documentation.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided)
#'
#'  - If FALSE, return the result of the [taxize::gna_verifier]
#'    function + a column taxa_names_in_phyloseq depicting the name of the
#'    taxa from the phyloseq object.
#'
#'  - If TRUE return a phyloseq object with amended slot `@taxtable`. Cannot be TRUE if `taxnames` is provided.
#'    Three new columns are added:
#'    - submittedName: The character string sent to gna_verifier (e.g.
#'    `Antrodiella brasiliensis`)
#'    - currentName: The current accepted name (resolve the synonymie) with
#'      autorities at the end of the binominal name (e.g.
#'      `Trametopsis brasiliensis (Ryvarden & de Meijer) Gómez-Mont. & Robledo)`.
#'    - currentCanonicalSimple: The current accepted name whithout autorities
#'      (e.g. `Trametopsis brasiliensis`).
#' @param genus_species_canonical_col (logical, default TRUE) If TRUE
#'   two new columns are added along with "currentCanonicalSimple":
#'   "currentCanonicalSimpleGenus" and "currentCanonicalSimpleSpecies"
#' @returns
#'   Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq object
#'   with 3 new columns (see param add_to_phyloseq) in the tax_table slot.
#' @export
#' @author Adrien Taudière
#'
#' @examples
#' gna_verifier_pq(data_fungi)
#'
#' gna_verifier_pq(data_fungi, data_sources = 210, add_to_phyloseq = FALSE)
#'
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
#'
#'
#' data_fungi_cleanNames <- gna_verifier_pq(data_fungi)
#'
#' sum(!is.na(data_fungi_cleanNames@tax_table[, "currentName"]))
#' sum(data_fungi_cleanNames@tax_table[, "currentCanonicalSimple"] != data_fungi_cleanNames@tax_table[, "submittedName"], na.rm = TRUE)
#' # 1010 taxa (71% of total) are identified using a currentName including 434
#' # corrected values (correction using synonym disambiguation)
#'
#' tr <- rotl_pq(data_fungi_cleanNames,
#'   taxonomic_rank = "currentCanonicalSimple",
#'   context_name = "Basidiomycetes"
#' )
#'
#' p <- ggtree::ggtree(tr, layout = "roundrect") +
#'   ggtree::geom_nodelab(hjust = 1, vjust = -1.2, size = 2) +
#'   ggtree::geom_tiplab(size = 2)
#'
#' p + xlim(0, max(p$data$x) + 1)
#'
#' # Return an error
#' # data_fungi_cleanNames <- gna_verifier_pq(data_fungi_cleanNames,
#' #                                        add_to_phyloseq=TRUE)
#' # data_fungi_cleanNames <- gna_verifier_pq(data_fungi,
#' #                                        taxonomic_rank="G_s")
#'
#' @details
#' This function is mainly a wrapper of the work of others.
#'   Please cite `taxize` package.
gna_verifier_pq <- function(physeq = NULL,
                            taxnames = NULL,
                            taxonomic_rank = c("Genus", "Species"),
                            data_sources = c(1, 12),
                            all_matches = FALSE,
                            capitalize = FALSE,
                            species_group = FALSE,
                            fuzzy_uninomial = FALSE,
                            stats = FALSE,
                            main_taxon_threshold = 0.5,
                            verbose = TRUE,
                            add_to_phyloseq = NULL,
                            genus_species_canonical_col = TRUE) {
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
      discard_genus_alone = FALSE,
      discard_NA = TRUE
    )
  }

  if (add_to_phyloseq) {
    if ("currentCanonicalSimple" %in% colnames(physeq@tax_table)) {
      stop(
        "The column currentCanonicalSimple is already present in the @tax_table
        slot of your phyloseq object. You should first delete or rename the
        superseed column before to rerun the function."
      )
    }
  }

  slice_taxnames <- if (length(taxnames) > 50) {
    # gna_verifier can show bugs when handle more than 49 names at a time
    split(taxnames, ceiling(seq_along(taxnames) / 49))
  } else {
    list(taxnames)
  }
  res_verifier <- bind_rows(lapply(slice_taxnames, function(x) {
    taxize::gna_verifier(x,
      data_sources = data_sources,
      all_matches = all_matches,
      capitalize = capitalize,
      species_group = species_group,
      fuzzy_uninomial = fuzzy_uninomial,
      stats = stats,
      main_taxon_threshold = main_taxon_threshold,
      output_type = "table"
    )
  }))

  res_verifier_clean <-
    res_verifier |>
    distinct() |>
    select(submittedName, currentName, currentCanonicalSimple)

  if (genus_species_canonical_col) {
    res_verifier_clean <- res_verifier_clean |>
      mutate(
        currentCanonicalSimpleGenus = stringr::str_split_i(currentCanonicalSimple, " ", 1),
        currentCanonicalSimpleSpecies = stringr::str_split_i(currentCanonicalSimple, " ", 2)
      )
  }

  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- cbind(as.data.frame(new_physeq@tax_table))
    tax_tab$taxa_name <-
      apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1,
        paste0,
        collapse = " "
      ) |>
      gsub(pattern = "NA NA", replacement = "") |>
      gsub(pattern = " NA", replacement = "")

    new_physeq@tax_table <-
      left_join(tax_tab, res_verifier_clean,
        by = join_by(taxa_name == submittedName)
      ) |>
      as.matrix() |>
      tax_table()

    taxtab_new <- new_physeq@tax_table |>
      as.data.frame() |>
      tibble()
    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    if (verbose) {
      total_taxa <- ntaxa(physeq)
      submitted_taxa <- sum(taxtab_new$taxa_name != "")
      genus_only_taxa <- sum(!grepl(" ", taxtab_new$taxa_name) & taxtab_new$taxa_name != "")
      total_matches <- sum(res_verifier$taxonomicStatus %in% c("Synonym", "Accepted"))
      synonyms <- sum(res_verifier$taxonomicStatus == "Synonym", na.rm = TRUE)
      genus_synonyms <- sum(res_verifier$matchedCardinality == 2 & res_verifier$taxonomicStatus == "Synonym", na.rm = TRUE)
      accepted_names <- sum(res_verifier$taxonomicStatus == "Accepted", na.rm = TRUE)
      genus_accepted <- sum(res_verifier$matchedCardinality == 2 & res_verifier$taxonomicStatus == "Accepted", na.rm = TRUE)

      cli::cli_bullets(c(
        "v" = "GNA verification summary:",
        "*" = "Total taxa in phyloseq: {.val {total_taxa}}",
        "*" = "Taxa submitted for verification: {.val {submitted_taxa}}",
        "*" = "Genus-level only taxa: {.val {genus_only_taxa}}",
        "*" = "Total matches found: {.val {total_matches}}",
        "*" = "Synonyms: {.val {synonyms}} (including {.val {genus_synonyms}} at genus level)",
        "*" = "Accepted names: {.val {accepted_names}} (including {.val {genus_accepted}} at genus level)"
      ))
    }
    return(new_physeq)
  } else {
    if (verbose) {
      total_matches <- sum(res_verifier$taxonomicStatus %in% c("Synonym", "Accepted"))
      synonyms <- sum(res_verifier$taxonomicStatus == "Synonym", na.rm = TRUE)
      genus_synonyms <- sum(res_verifier$matchedCardinality == 2 & res_verifier$taxonomicStatus == "Synonym", na.rm = TRUE)
      accepted_names <- sum(res_verifier$taxonomicStatus == "Accepted", na.rm = TRUE)
      genus_accepted <- sum(res_verifier$matchedCardinality == 2 & res_verifier$taxonomicStatus == "Accepted", na.rm = TRUE)

      cli::cli_bullets(c(
        "v" = "GNA verification summary:",
        "*" = "Taxa submitted for verification: {.val {length(taxnames)}}",
        "*" = "Total matches found: {.val {total_matches}}",
        "*" = "Synonyms: {.val {synonyms}} (including {.val {genus_synonyms}} at genus level)",
        "*" = "Accepted names: {.val {accepted_names}} (including {.val {genus_accepted}} at genus level)"
      ))
    }
    res_verifier$taxa_names_in_phyloseq <- names(taxnames)
    return(res_verifier)
  }
}
