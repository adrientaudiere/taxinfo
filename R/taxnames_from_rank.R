#' Build per-taxon names from taxonomic rank column(s)
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Internal key-builder shared by [taxonomic_rank_to_taxnames()] (the query
#' side) and [augment_tax_table()] (the merge side). It pastes the
#' `taxonomic_rank` column(s) of a `tax_table` into a single name per taxon and
#' applies the same `"NA NA"` / `" NA"` cleanup, so that the names sent to an
#' external database and the join key used to merge the results back are
#' guaranteed to be identical.
#'
#' Unlike [taxonomic_rank_to_taxnames()], this helper never discards or
#' de-duplicates rows: it returns exactly one (possibly empty) string per taxon,
#' aligned to `rownames(tax_table)`.
#'
#' @param tax_table A `tax_table` (taxonomyTable) or character matrix.
#' @param taxonomic_rank (character) The column(s) of `tax_table` to paste
#'  together, in order (e.g. `"currentCanonicalSimple"` or
#'  `c("Genus", "Species")`).
#' @param clean (logical, default `TRUE`) If `TRUE`, drop the `"NA"` tokens
#'  produced when a rank cell is missing (`"Amanita NA"` becomes `"Amanita"`,
#'  `"NA NA"` becomes `""`) and trim surrounding whitespace.
#'
#' @returns An unnamed character vector with one element per taxon.
#'
#' @author Adrien Taudiere
#' @keywords internal
#' @seealso [taxonomic_rank_to_taxnames()], [augment_tax_table()]
taxnames_from_rank <- function(tax_table, taxonomic_rank, clean = TRUE) {
  mat <- as.matrix(unclass(tax_table))[, taxonomic_rank, drop = FALSE]
  key <- apply(mat, 1, paste0, collapse = " ")

  if (clean) {
    key <- gsub("NA NA", "", key)
    key <- gsub(" NA", "", key)
    key <- trimws(key)
  }

  as.vector(key)
}
