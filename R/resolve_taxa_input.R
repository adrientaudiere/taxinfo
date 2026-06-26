#' Resolve the phyloseq-or-taxnames input of a `tax_*_pq()` function
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Internal helper shared by the `tax_*_pq` family. It validates the mutually
#' exclusive `physeq` / `taxnames` input, resolves the default for
#' `add_to_phyloseq`, and extracts the taxon names from `physeq` (via
#' [taxonomic_rank_to_taxnames()]) when `taxnames` is not supplied. It is the
#' "front-matter" counterpart of the "merge-back" helper [augment_tax_table()].
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames`
#'  must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param add_to_phyloseq (logical or `NULL`) The caller's `add_to_phyloseq`
#'  argument. When `NULL`, it defaults to `TRUE` if `physeq` is provided and
#'  `FALSE` otherwise. Aborts if `TRUE` while `taxnames` is supplied. Pass
#'  `NA` for callers that have no `add_to_phyloseq` concept (the returned value
#'  is then meaningless and can be ignored).
#' @param taxonomic_rank (character) The column(s) of `physeq@tax_table` used to
#'  build the taxon names. Forwarded to [taxonomic_rank_to_taxnames()].
#' @param discard_genus_alone,discard_NA (logical) Forwarded to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns A list with two elements: `taxnames` (the resolved character vector)
#'  and `add_to_phyloseq` (the resolved logical).
#'
#' @author Adrien Taudiere
#' @keywords internal
#' @seealso [augment_tax_table()], [taxonomic_rank_to_taxnames()]
resolve_taxa_input <- function(
  physeq = NULL,
  taxnames = NULL,
  add_to_phyloseq = NULL,
  taxonomic_rank,
  discard_genus_alone = FALSE,
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

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = discard_genus_alone,
      discard_NA = discard_NA
    )
  }

  list(taxnames = taxnames, add_to_phyloseq = add_to_phyloseq)
}
