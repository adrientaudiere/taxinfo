#' Select taxa in a phyloseq object based on names in a given column of the tax_table
#'
#' @param physeq A phyloseq object
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'  The column(s) present in the @tax_table slot of the phyloseq object. Can
#'  be a vector of two columns (e.g. c("Genus", "Species")).
#' @param taxnames (A character vector of taxonomic names to select)
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param clean_pq (logical, default FALSE) If TRUE, clean the phyloseq object
#' after subsetting (i.e. remove empty taxa and samples). If FALSE, only
#' empty taxa are removed to take all samples.
#' @param ... Additional arguments to pass to [subset_taxa_pq()].
#'
#' @returns A new phyloseq object containing only the selected taxa.
#' @export
#'
#' @author Adrien Taudière
#'
#' @examples
#' select_taxa_pq(data_fungi_mini_cleanNames, taxonomic_rank = "currentCanonicalSimple", taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"), verbose = FALSE, clean_pq = FALSE)
#' select_taxa_pq(data_fungi, taxonomic_rank = c("Genus", "Species"), taxnames = c("Xylodon flaviporus"), verbose = FALSE, clean_pq = FALSE)
#'
#' select_taxa_pq(data_fungi, taxonomic_rank = "Trait", taxnames = c("Soft Rot")) |>
#'   summary_plot_pq()
select_taxa_pq <- function(physeq, taxonomic_rank = "currentCanonicalSimple", taxnames = NULL, verbose = TRUE, clean_pq = FALSE, ...) {
  verify_pq(physeq, verbose = verbose)

  taxnames_in_physeq <- apply(physeq@tax_table[, taxonomic_rank], 1, paste, collapse = " ")

  cond <- taxnames_in_physeq %in% taxnames
  names(cond) <- taxa_names(physeq)
  new_physeq <- subset_taxa_pq(physeq, cond, verbose = verbose, clean_pq = clean_pq, ...) |>
    clean_pq(silent = !verbose, remove_empty_samples = FALSE)

  return(new_physeq)
}
