#' Extract taxonomic names from a phyloseq object
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#'   Mainly a internal function for function [gna_verifier_pq()], [tax_oa_pq()],
#'  [gbif_occur_pq()], [tax_iucn_code_pq()], [tax_globi_pq()],
#'  [plot_tax_gbif_pq()], ...
#' @param physeq A phyloseq object
#' @param taxonomic_rank (Character)
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. the default c("Genus", "Species")).
#' @param discard_genus_alone (logical default FALSE). If TRUE genus
#'  without information at the species level are discarded.
#' @param discard_NA (logical default TRUE). If TRUE, taxa with NA in the
#'  taxonomic_rank are discarded.
#' @param distinct_names (logical default TRUE). If TRUE, return only unique
#' taxonomic names.
#'
#' @returns A vector of unique taxonomic names
#' @export
#' @author Adrien Taudiere
#'
#' @examples
#' taxonomic_rank_to_taxnames(data_fungi_mini)
#' taxonomic_rank_to_taxnames(data_fungi_mini, discard_genus_alone = TRUE)
#' taxonomic_rank_to_taxnames(data_fungi_mini, discard_NA = TRUE)
#' taxonomic_rank_to_taxnames(data_fungi_mini,
#'   discard_NA = TRUE, discard_genus_alone = TRUE
#' )
#'
#' \dontrun{
#' taxonomic_rank_to_taxnames(gna_verifier_pq(data_fungi_mini), taxonomic_rank="currentCanonicalSimple")
#' taxonomic_rank_to_taxnames(gna_verifier_pq(data_fungi_mini), taxonomic_rank="genusEpithet")
#' }
#' taxonomic_rank_to_taxnames(data_fungi_mini, taxonomic_rank="Class")
#' taxonomic_rank_to_taxnames(data_fungi_mini, taxonomic_rank="Class",
#'  distinct_names = FALSE,
#'  discard_NA = TRUE
#' )
taxonomic_rank_to_taxnames <- function(
  physeq,
  taxonomic_rank = c("Genus", "Species"),
  discard_genus_alone = FALSE,
  discard_NA = TRUE,
  distinct_names = TRUE
) {
  verify_pq(physeq)

  if (sum(!taxonomic_rank %in% colnames(physeq@tax_table)) != 0) {
    stop(
      "The taxonomic_rank parameter do not fit with the @tax_table column of your phyloseq object."
    )
  }

  taxnames <- taxnames_from_rank(
    physeq@tax_table,
    taxonomic_rank,
    clean = discard_NA
  )
  if (discard_genus_alone) {
    taxnames <- taxnames[grepl(pattern = " ", taxnames)]
  }
  if (discard_NA) {
    taxnames <- taxnames[taxnames != ""]
  }
  if (distinct_names) {
    taxnames <- unique(taxnames)
  }
  taxnames <- as.vector(taxnames)
  return(taxnames)
}
