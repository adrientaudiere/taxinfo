#' Count GBIF occurrences per ecoregion for the taxa of a phyloseq object
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Phyloseq wrapper around [tax_ecoregion_occur()]. Extracts taxon names from
#' `physeq` using the column(s) named in `taxonomic_rank` (default
#' `"currentCanonicalSimple"`, the output of [gna_verifier_pq()]; use
#' `"genusSpeciesEpithet"` to match the column produced by
#' `gna_verifier_pq(..., genus_species_canonical_col = TRUE)`), then queries
#' GBIF and maps occurrences to WWF/TNC terrestrial ecoregions.
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames`
#'  must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (character, default `"currentCanonicalSimple"`). The
#'  column(s) of `physeq@tax_table` to paste together as taxon names.
#' @param add_to_phyloseq (logical, default `TRUE` when `physeq` is provided,
#'  `FALSE` otherwise). If `TRUE`, add three columns
#'  (`<col_prefix>ecoregion_top`, `<col_prefix>ecoregion_n`,
#'  `<col_prefix>ecoregion_list`) to `physeq@tax_table` and return the
#'  updated phyloseq object. If `FALSE`, return the long tibble from
#'  [tax_ecoregion_occur()].
#' @param col_prefix (character, default `NULL`). Prefix for the new tax_table
#'  columns. Defaults to `"ecoregion_"` if `NULL` (yielding
#'  `ecoregion_top` / `ecoregion_n` / `ecoregion_list`).
#' @inheritParams tax_ecoregion_occur
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns Either a phyloseq object with three new tax_table columns (if
#'  `add_to_phyloseq = TRUE`) or the long tibble produced by
#'  [tax_ecoregion_occur()] (otherwise). In the latter case,
#'  `attr(result, "tax_summary")` holds the one-row-per-taxon summary used to
#'  build the phyloseq columns.
#'
#' @author Adrien Taudiere
#' @seealso [tax_ecoregion_occur()], [tax_check_ecoregion()],
#'  [taxonomic_rank_to_taxnames()]
#' @examples
#' \dontrun{
#' data_fungi_mini_clean <- gna_verifier_pq(data_fungi_mini)
#' tax_ecoregion_occur_pq(
#'   data_fungi_mini_clean,
#'   taxonomic_rank = "genusSpeciesEpithet",
#'   n_occur = 100
#' )
#' }
#' @export
tax_ecoregion_occur_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  n_occur = 1000,
  min_nb_occur = 0,
  min_proportion = 0,
  clean_coord = FALSE,
  verbose = TRUE,
  time_to_sleep = 0.3,
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

  if (is.null(col_prefix)) {
    col_prefix <- "ecoregion_"
  }

  long_tbl <- tax_ecoregion_occur(
    taxnames = taxnames,
    n_occur = n_occur,
    min_nb_occur = min_nb_occur,
    min_proportion = min_proportion,
    clean_coord = clean_coord,
    verbose = verbose,
    time_to_sleep = time_to_sleep
  )

  summary_tbl <- long_tbl |>
    dplyr::filter(!is.na(.data$ECO_NAME)) |>
    dplyr::arrange(.data$taxon_name, dplyr::desc(.data$n_occur)) |>
    dplyr::group_by(.data$taxon_name) |>
    dplyr::summarise(
      ecoregion_top = dplyr::first(.data$ECO_NAME),
      ecoregion_n = dplyr::n(),
      ecoregion_list = paste(.data$ECO_NAME, collapse = "; "),
      .groups = "drop"
    )

  all_taxa <- tibble::tibble(taxon_name = unique(taxnames))
  summary_tbl <- all_taxa |>
    dplyr::left_join(summary_tbl, by = "taxon_name") |>
    dplyr::mutate(
      ecoregion_n = dplyr::coalesce(.data$ecoregion_n, 0L)
    )

  names(summary_tbl)[names(summary_tbl) != "taxon_name"] <- paste0(
    col_prefix,
    c("top", "n", "list")
  )

  if (!add_to_phyloseq) {
    attr(long_tbl, "tax_summary") <- summary_tbl
    return(long_tbl)
  }

  augment_tax_table(
    physeq,
    summary_tbl,
    taxonomic_rank = taxonomic_rank,
    info_key = "taxon_name",
    keep_key = FALSE
  )
}
