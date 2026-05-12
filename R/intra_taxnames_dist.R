#' Compute intra-taxanames distances for each taxa names
#'
#' @description
#' This function computes intra-taxanames distances for each taxonomic names
#'  (e.g. Genus species) in a phyloseq object containing ASV/OTU sequences and taxonomy.
#'
#' The distances are computed using the DECIPHER package, which aligns the sequences
#' (`DECIPHER::AlignSeqs()`) and calculates a distance matrix (`DECIPHER::DistanceMatrix()`).
#'
#' @param physeq A phyloseq object containing ASV/OTU sequences and refseq
#' @param taxonomic_rank Character. Name of the taxonomy column(s) containing
#'  taxonomic assignments to compute intra-taxa distances. Can be a vector of
#'  two columns (e.g. c("Genus", "Species"), the default).
#' @param verbose Logical. Print progress messages (default: TRUE)
#' @param verbose_DECIPHER Logical. If TRUE, print messages from DECIPHER functions (default: FALSE)
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param ... Additional arguments to pass to `DECIPHER::AlignSeqs()`
#' 
#' @returns A data.frame with columns:
#' - taxnames: taxonomic names
#' - n_taxa: number of taxa assigned to this taxnames
#' - mean_dist: mean intra-taxanames distance
#' - min_dist: minimum intra-taxanames distance
#' - max_dist: maximum intra-taxanames distance
#'
#' @export
#' @author Adrien Taudiere
#' @seealso [DECIPHER::AlignSeqs()], [DECIPHER::DistanceMatrix()]
#' @examples
#' intra_taxn_dist <- intra_taxnames_dist(data_fungi_mini)
#' plot(intra_taxn_dist$mean_dist, intra_taxn_dist$n_taxa)
#' plot(intra_taxn_dist$min_dist, intra_taxn_dist$n_taxa)
#' plot(intra_taxn_dist$max_dist, intra_taxn_dist$n_taxa)
intra_taxnames_dist <- function(
  physeq,
  taxonomic_rank = c("Genus", "Species"),
  verbose = TRUE,
  verbose_DECIPHER = FALSE,
  discard_NA = TRUE,
  ...
) {
  taxnames <- taxonomic_rank_to_taxnames(
    physeq,
    taxonomic_rank,
    discard_genus_alone = TRUE,
    discard_NA = discard_NA
  )
  taxnames_all <- apply(
    physeq@tax_table[, taxonomic_rank],
    1,
    paste,
    collapse = " "
  )

  results <- data.frame(
    taxnames = taxnames,
    n_taxa = sapply(taxnames, function(x) {
      sum(
        taxonomic_rank_to_taxnames(
          physeq,
          taxonomic_rank,
          distinct_names = FALSE,
          discard_NA = discard_NA
        ) ==
          x
      )
    }),
    mean_dist = NA_real_,
    min_dist = NA_real_,
    max_dist = NA_real_
  )
  for (taxn in taxnames) {
    selected_taxa <- taxa_names(physeq)[taxnames_all == taxn]
    if (length(selected_taxa) == 1) {
      if (verbose) {
        cli::cli_alert_info(" {.emph {taxn}} is represented by only one taxa")
      }
      next
    } else {
      if (verbose) {
        cli::cli_alert_success(
          "Processing {.emph {taxn}} - {.val {length(selected_taxa)}} taxa"
        )
      }
    }

    dist_matrix <- DECIPHER::AlignSeqs(
      physeq@refseq[selected_taxa],
      verbose = verbose_DECIPHER,
      ...
    ) |>
      DECIPHER::DistanceMatrix(verbose = verbose_DECIPHER)
    half_mat <- dist_matrix[upper.tri(dist_matrix)]

    results$mean_dist[results$taxnames == taxn] <- mean(half_mat)
    results$min_dist[results$taxnames == taxn] <- min(half_mat)
    results$max_dist[results$taxnames == taxn] <- max(half_mat)
  }
  if (verbose) {
    cli::cli_alert_success("Intra-taxnames distance computation complete")
    cli::cli_alert_info("Total taxnames: {.val {nrow(results)}}")
    cli::cli_alert_info(
      "Taxnames with only one taxa (no distance computation):  {.val {sum(results$n_taxa==1)}}"
    )
    cli::cli_alert_info(
      "Taxnames with multiple taxa: {.val {sum(results$n_taxa>1)}}"
    )
    cli::cli_alert_info(
      "Mean intra-taxnames mean distance: {.val {round(mean(results$mean_dist, na.rm=TRUE),4)}}"
    )
    cli::cli_alert_info(
      "Mean intra-taxnames maximum distance: {.val {round(mean(results$max_dist, na.rm=TRUE),4)}}"
    )
    cli::cli_alert_info(
      "Mean intra-taxnames minimum distance: {.val {round(mean(results$min_dist, na.rm=TRUE),4)}}"
    )
  }
  return(results)
}
