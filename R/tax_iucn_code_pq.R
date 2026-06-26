#' Get iucn conservation status through gbif
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided)
#'  If TRUE, add a new column (iucn_code) in the tax_table of the phyloseq object.
#'  Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#'  Cannot be TRUE if `taxnames` is provided.
#' @param col_prefix A character string to be added as a prefix to the new
#' columns names added to the tax_table slot of the phyloseq object (default: NULL).
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#' object, if add_to_phyloseq = TRUE, with 1 new column (iucn_code) in the
#' tax_table.
#'
#' @export
#' @author Adrien Taudiere
#' @details
#' This function is mainly a wrapper of the work of others.
#'   Please cite `rgbif` package.
#' @seealso [tax_info_pq()], [rgbif::name_usage()]
#' @examples
#' \dontrun{
#'
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini) |>
#'   tax_iucn_code_pq()
#'
#' table(data_fungi_mini_cleanNames@tax_table[, "iucn_code"])
#'
#' # Using taxnames vector (returns a tibble)
#' tax_iucn_code_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
#' }
tax_iucn_code_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
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

  # Get IUCN Red List category for each taxon in the backbone
  iucn_codes <- sapply(gbif_taxa$usageKey, function(x) {
    rgbif::name_usage(x, data = "iucnRedListCategory")$data$code
  })
  iucn_codes_df <- data.frame(
    "iucn_code" = iucn_codes,
    "taxa_name" = gbif_taxa$verbatim_name
  )

  if (add_to_phyloseq) {
    return(augment_tax_table(
      physeq,
      iucn_codes_df,
      taxonomic_rank = taxonomic_rank,
      col_prefix = col_prefix,
      default_prefix = "iucn_"
    ))
  } else {
    return(iucn_codes_df)
  }
}
