#' Get iucn conservation status through gbif
#'
#' @param physeq A phyloseq object
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param add_to_phyloseq (logical, default FALSE) If TRUE, add a new column
#'  (iucn_code) in the tax_table of the phyloseq object.
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#' object, if add_to_phyloseq = TRUE, with 1 new column (iucn_code) in the
#' tax_table.
#'
#' @export
#' @author Adrien Taudière
#' @details
#' This function is mainly a wrapper of the work of others.
#'   Please cite `rgbif` package.
#' @examples
#' data_fungi_mini_cleanNames <-
#'   gna_verifier_pq(data_fungi_mini, add_to_phyloseq = TRUE)
#' tax_iucn_code_pq(data_fungi_mini_cleanNames)
#' data_fungi_mini_cleanNames <- tax_iucn_code_pq(data_fungi_mini_cleanNames,
#'   add_to_phyloseq = TRUE
#' )
#' table(data_fungi_mini_cleanNames@tax_table[, "iucn_code"])
tax_iucn_code_pq <- function(physeq,
                             taxonomic_rank = "currentCanonicalSimple",
                             add_to_phyloseq = FALSE) {
  taxnames <- taxonomic_rank_to_taxnames(
    physeq = physeq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = TRUE
  )

  gbif_taxa <- rgbif::name_backbone_checklist(taxnames) |>
    filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
    distinct()

  # Get IUCN Red List category for each taxon in the backbone
  iucn_codes <- sapply(gbif_taxa$usageKey, function(x) {
    rgbif::name_usage(x, data = "iucnRedListCategory")$data$code
  })
  iucn_codes_df <- data.frame(
    "iucn_code" = iucn_codes,
    "taxa_name" = gbif_taxa$canonicalName
  )

  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")
    new_physeq@tax_table <-
      full_join(tax_tab, iucn_codes_df) |>
      as.matrix() |>
      tax_table()
    rownames(new_physeq@tax_table) <- taxa_names(physeq)
    return(new_physeq)
  } else {
    return(iucn_codes_df)
  }
}
