#' Count GBIF occurrences per ecoregion for the taxa of a phyloseq object
#'
#' @description
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
#' data(data_fungi_mini)
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
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort(
      "You must specify either {.arg physeq} or {.arg taxnames}, not both"
    )
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }

  if (is.null(add_to_phyloseq)) {
    add_to_phyloseq <- !is.null(physeq)
  }
  if (!is.null(taxnames) && isTRUE(add_to_phyloseq)) {
    cli::cli_abort(
      "{.arg add_to_phyloseq} cannot be TRUE when {.arg taxnames} is provided"
    )
  }
  if (is.null(col_prefix)) {
    col_prefix <- "ecoregion_"
  }

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = discard_genus_alone,
      discard_NA = discard_NA
    )
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

  new_physeq <- physeq
  tax_tab <- as.data.frame(new_physeq@tax_table)
  tax_tab$taxa_name <- apply(
    unclass(new_physeq@tax_table[, taxonomic_rank]),
    1,
    paste0,
    collapse = " "
  )

  existing_cols <- intersect(
    colnames(tax_tab),
    names(summary_tbl)[-1]
  )
  if (length(existing_cols) > 0) {
    cli::cli_warn(c(
      "Overwriting existing tax_table column{?s}: {.val {existing_cols}}",
      "i" = "Pass {.arg col_prefix} to avoid the conflict"
    ))
    tax_tab <- tax_tab[,
      setdiff(colnames(tax_tab), existing_cols),
      drop = FALSE
    ]
  }

  new_tax_tab <- dplyr::left_join(
    tax_tab,
    summary_tbl,
    by = dplyr::join_by("taxa_name" == "taxon_name")
  )
  new_tax_tab$taxa_name <- NULL

  new_physeq@tax_table <- phyloseq::tax_table(as.matrix(new_tax_tab))
  rownames(new_physeq@tax_table) <- phyloseq::taxa_names(physeq)

  new_physeq
}
