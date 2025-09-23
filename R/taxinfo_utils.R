#' Calculate Bounding Box Around a Point
#'
#' @description
#'
#' This function calculates a geographic bounding box around a given point
#' with a specified radius in kilometers.
#'
#' @param longitude Numeric. Longitude of the center point in decimal degrees.
#' @param latitude Numeric. Latitude of the center point in decimal degrees.
#' @param radius_km Numeric. Radius in kilometers for the bounding box.
#'
#' @return A list containing xmin, xmax, ymin, ymax coordinates in decimal degrees.
#'
#' @details The function uses an approximation where 1 degree ~= 111.32 km and
#'   adjusts for latitude distortion where longitude degrees get closer at the poles.
#'
#' @author Adrien Taudière
#' @examples
#' bbox <- calculate_bbox(2.3522, 48.8566, 50)
#' bbox
#'
#' @keywords internal
calculate_bbox <- function(longitude = NULL, latitude = NULL, radius_km = 1) {
  # Approximation: 1 degree ~= 111.32 km
  lat_offset <- radius_km / 111.32
  # Adjustment for latitude (longitude degrees get closer at the poles)
  lon_offset <- radius_km / (111.32 * cos(latitude * pi / 180))

  res <- list(
    "xmin" = longitude - lon_offset,
    "xmax" = longitude + lon_offset,
    "ymin" = latitude - lat_offset,
    "ymax" = latitude + lat_offset
  )

  return(res)
}


#' Text summary for a taxonomic rank
#'
#' @description
#' Create a text to summarize the number of samples, taxa, sequences and occurrences of selected taxa in a phyloseq object for a given value in the column of a tax_table
#'
#' @param physeq A phyloseq object
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'  The column(s) present in the @tax_table slot of the phyloseq object. Can
#'  be a vector of two columns (e.g. c("Genus", "Species")).
#' @param taxnames (A character vector of taxonomic names to select)
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param min_nb_seq minimum number of sequences by OTUs by
#'   samples to take into count this OTUs in this sample. For example,
#'   if min_nb_seq=2,each value of 2 or less in the OTU table
#'   will not count in the venn diagram
#' @param ... Additional arguments to pass to [subset_taxa_pq()].
#' @author Adrien Taudière
#'
#' @returns A character string summarizing the number of samples, taxa, sequences and occurrences of the selected taxa.
#' @export
#'
#' @examples
#' taxa_summary_text(data_fungi_cleanNames, taxnames = c("Xylodon raduloides"))
#' taxa_summary_text(data_fungi_cleanNames,
#'   taxnames = c("Xylodon raduloides"),
#'   min_nb_seq = 100, verbose = FALSE
#' )
#' taxa_summary_text(data_fungi_cleanNames,
#'   taxonomic_rank = "Trait",
#'   taxnames = c("Soft Rot"), verbose = FALSE
#' )
taxa_summary_text <- function(physeq, taxonomic_rank = "currentCanonicalSimple", taxnames = NULL, verbose = TRUE, min_nb_seq = 0, ...) {
  new_physeq <- select_taxa_pq(physeq = physeq, taxonomic_rank = taxonomic_rank, taxnames = taxnames, verbose = verbose, clean_pq = FALSE, ...) |> clean_pq(silent = T)

  if (min_nb_seq > 0) {
    new_physeq@otu_table[new_physeq@otu_table < min_nb_seq] <- 0
    new_physeq2 <- clean_pq(new_physeq, silent = TRUE)
    if (verbose) {
      message(paste0(
        "Filtering OTUs presences with less than ", min_nb_seq, " sequences remove",
        nsamples(new_physeq) -
          nsamples(new_physeq2), " samples, ", ntaxa(new_physeq) - ntaxa(new_physeq2), " taxa, ", sum(new_physeq@otu_table) - sum(new_physeq2@otu_table), " sequences and ", sum(new_physeq@otu_table > 0) - sum(new_physeq2@otu_table > 0), " occurrences."
      ))
    }
    new_physeq <- new_physeq2
  }

  nsamp <- nsamples(new_physeq)
  ntaxa <- ntaxa(new_physeq)
  nseq <- sum(new_physeq@otu_table)
  noccur <- sum(new_physeq@otu_table > 0)

  paste0(taxnames, ": ", nsamp, " samp., ", ntaxa, " taxa, ", nseq, " seq., ", noccur, " occ.")
}
