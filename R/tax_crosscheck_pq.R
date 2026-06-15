#' Cross-check taxonomic names using GBIF backbone and GNA Verifier
#'
#' @description
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Compares name-verification results from two independent sources:
#' \itemize{
#'   \item \strong{GNA Verifier} (via [taxize::gna_verifier()]) with
#'     `data_sources = 11` (GBIF Backbone Taxonomy)
#'   \item \strong{rgbif backbone} (via [rgbif::name_backbone_checklist()])
#' }
#'
#' Because the two services use different matching algorithms and update
#' schedules, discrepancies highlight taxa that may need manual review.
#' A Venn-style summary shows the overlap in matched canonical names.
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or
#'   `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank Character vector. The column(s) in the
#'   `@tax_table` slot used to construct taxon names when `physeq` is
#'   provided. Default `c("Genus", "Species")`.
#' @param data_sources Integer or character vector passed to
#'   [taxize::gna_verifier()]. Default `11` (GBIF Backbone Taxonomy).
#'   Use `c(1, 11)` to also include Catalogue of Life, for example.
#' @param plot (logical, default `TRUE`). If `TRUE` and
#'   \pkg{ggVennDiagram} is installed, a Venn diagram of the two sets
#'   of matched canonical names is included in the returned list.
#' @param verbose (logical, default `TRUE`). Print progress messages.
#' @param ... Additional arguments passed to [gna_verifier_pq()].
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{gna_results}: tibble returned by [gna_verifier_pq()]
#'       (with `add_to_phyloseq = FALSE`).
#'     \item \code{backbone_results}: tibble returned by
#'       [rgbif::name_backbone_checklist()].
#'     \item \code{comparison}: data.frame with one row per submitted
#'       taxon, columns for the canonical name from each source, and a
#'       \code{status} column (\code{"match"}, \code{"mismatch"},
#'       \code{"gna_only"}, \code{"backbone_only"}, or \code{"both_na"}).
#'     \item \code{summary}: named numeric vector with counts of each
#'       status category.
#'     \item \code{venn_plot}: (optional) a \pkg{ggVennDiagram} object
#'       comparing the two sets of matched canonical names.
#'   }
#' @export
#' @author Adrien Taudière
#'
#' @seealso [gna_verifier_pq()], [rgbif::name_backbone_checklist()]
#'
#' @examples
#' \dontrun{
#' # Cross-check a phyloseq object
#' res <- tax_crosscheck_pq(data_fungi)
#' res$summary
#' res$comparison |> filter(status == "mismatch")
#'
#' res$venn_plot 
#' 
#' res_taxref <- tax_crosscheck_pq(data_fungi, data_sources = 12)
#' 
#' # Cross-check a vector of names
#' res2 <- tax_crosscheck_pq(taxnames = c(
#'   "Trametopsis brasiliensis",
#'   "Fake species Waller 2022",
#'   "Russula"
#' ))
#' res2$summary
#' }
tax_crosscheck_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = c("Genus", "Species"),
  data_sources = 11,
  plot = TRUE,
  verbose = TRUE,
  ...
) {
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort(
      "You must specify either {.arg physeq} or {.arg taxnames}, not both"
    )
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }

  # Resolve taxnames from phyloseq if needed
  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = FALSE,
      discard_NA = TRUE
    )
  }

  if (verbose) {
    cli::cli_alert_info(
      "Running GNA Verifier (data_sources = {.val {data_sources}}) on {.val {length(taxnames)}} taxa..."
    )
  }

  # Run GNA Verifier
  gna_res <- gna_verifier_pq(
    taxnames = taxnames,
    data_sources = data_sources,
    add_to_phyloseq = FALSE,
    verbose = verbose,
    authorship_col = FALSE,
    ...
  )

  if (verbose) {
    cli::cli_alert_info(
      "Running rgbif::name_backbone_checklist() on {.val {length(taxnames)}} taxa..."
    )
  }

  # Run GBIF backbone checklist
  backbone_res <- rgbif::name_backbone_checklist(taxnames)

  # Extract canonical names for comparison
  gna_names <- gna_res$currentCanonicalSimple
  backbone_names <- backbone_res$species

  # Build comparison data.frame
  comparison <- data.frame(
    submitted_name = taxnames,
    gna_canonical = gna_names,
    backbone_canonical = backbone_names,
    stringsAsFactors = FALSE
  )

  # Classify each taxon
  comparison$status <- mapply(
    function(g, b) {
      g_na <- is.na(g) || g == ""
      b_na <- is.na(b) || b == ""
      if (g_na && b_na) {
        return("both_na")
      } else if (g_na) {
        return("backbone_only")
      } else if (b_na) {
        return("gna_only")
      } else if (g == b) {
        return("match")
      } else {
        return("mismatch")
      }
    },
    gna_names,
    backbone_names,
    USE.NAMES = FALSE
  )

  # Summary counts
  status_counts <- table(comparison$status)
  summary_vec <- c(
    total = nrow(comparison),
    match = sum(comparison$status == "match"),
    mismatch = sum(comparison$status == "mismatch"),
    gna_only = sum(comparison$status == "gna_only"),
    backbone_only = sum(comparison$status == "backbone_only"),
    both_na = sum(comparison$status == "both_na")
  )

  if (verbose) {
    cli::cli_bullets(c(
      "v" = "Cross-check summary:",
      "*" = "Total taxa: {.val {summary_vec[['total']]}}",
      "*" = "Matches: {.val {summary_vec[['match']]}}",
      "*" = "Mismatches: {.val {summary_vec[['mismatch']]}}",
      "*" = "GNA only: {.val {summary_vec[['gna_only']]}}",
      "*" = "Backbone only: {.val {summary_vec[['backbone_only']]}}",
      "*" = "Both NA: {.val {summary_vec[['both_na']]}}"
    ))
  }

  res <- list(
    gna_results = gna_res,
    backbone_results = backbone_res,
    comparison = comparison,
    summary = summary_vec
  )

  # Optional Venn diagram
  if (plot) {
    if (requireNamespace("ggVennDiagram", quietly = TRUE)) {
      gna_set <- unique(gna_names[!is.na(gna_names) & gna_names != ""])
      backbone_set <- unique(backbone_names[!is.na(backbone_names) & backbone_names != ""])

      res$venn_plot <- ggVennDiagram::ggVennDiagram(
        list(
          GNA_Verifier = gna_set,
          GBIF_Backbone = backbone_set
        )
      ) +
        ggplot2::ggtitle("Matched canonical names: GNA Verifier vs GBIF Backbone")
    } else {
      if (verbose) {
        cli::cli_alert_info(
          "Install {.pkg ggVennDiagram} to generate a Venn diagram: ",
          "{.code install.packages('ggVennDiagram')}"
        )
      }
    }
  }

  return(res)
}
