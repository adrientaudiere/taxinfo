#' Get iucn conservation status through gbif
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
#'   gna_verifier_pq(data_fungi_mini) |>
#'   tax_iucn_code_pq(data_fungi_mini_cleanNames)
#'
#' table(data_fungi_mini_cleanNames@tax_table[, "iucn_code"])
#'
#' # Using taxnames vector (returns a tibble)
#' tax_iucn_code_pq(taxnames = c("Amanita muscaria", "Boletus edulis"))
tax_iucn_code_pq <- function(physeq = NULL,
                             taxnames = NULL,
                             taxonomic_rank = "currentCanonicalSimple",
                             add_to_phyloseq = NULL,
                             col_prefix = NULL) {
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
      discard_genus_alone = TRUE
    )
  }

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

  # Determine new column names (excluding taxa_name which is used for join)
  new_cols <- c("iucn_code")

  # Check for column name collisions and handle col_prefix
  if (add_to_phyloseq) {
    existing_cols <- colnames(physeq@tax_table)
    common_cols <- intersect(paste0(col_prefix, new_cols), existing_cols)

    if (length(common_cols) > 0 && is.null(col_prefix)) {
      cli::cli_warn(c(
        "Column names already exist in tax_table: {.val {common_cols}}",
        "i" = "Adding prefix 'iucn_' to avoid conflicts"
      ))
      col_prefix <- "iucn_"
    }
  }

  # Apply col_prefix to new columns
  if (!is.null(col_prefix)) {
    iucn_codes_df <- iucn_codes_df |>
      rename_with(~ paste0(col_prefix, .), .cols = -taxa_name)
  }

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
