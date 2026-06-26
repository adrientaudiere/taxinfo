#' Merge per-taxon information into the tax_table of a phyloseq object
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Internal helper shared by the `tax_*_pq` family. It owns the "merge-back"
#' step common to every function that augments a phyloseq object with
#' externally-fetched, per-taxon information: build the join key from the
#' `taxonomic_rank` column(s), handle `col_prefix` collisions, left-join the
#' information tibble, and rebuild the `tax_table` slot while preserving the
#' original taxa order and names. Each `tax_*_pq` function only has to produce
#' `info_tbl`; the external fetch stays in the caller.
#'
#' @details
#' **Key invariant.** `info_tbl` must be keyed (column `info_key`) by the
#' *submitted query name* -- the value produced by [taxnames_from_rank()] /
#' [taxonomic_rank_to_taxnames()] that was sent to the external database -- and
#' **not** by whatever name the database returned. Keying on the query name is
#' what makes the join correct without re-verifying names: both sides live in
#' the same namespace by construction. This module is deliberately network-free
#' and does not call [gna_verifier_pq()]; name harmonisation belongs upstream
#' (run [gna_verifier_pq()] first and pass
#' `taxonomic_rank = "currentCanonicalSimple"`) or inside the caller's fetch.
#'
#' The join is a `left_join` with `relationship = "many-to-one"`: every taxon is
#' kept (unmatched taxa get `NA`), and a duplicated key in `info_tbl` is a hard
#' error rather than a silent row multiplication.
#'
#' @param physeq (required) A phyloseq object.
#' @param info_tbl (required) A tibble or data frame with one row per taxon,
#'  keyed by `info_key`. All other columns are added to the `tax_table`.
#' @param taxonomic_rank (character) The column(s) of `physeq@tax_table` whose
#'  pasted value (via [taxnames_from_rank()]) forms the join key.
#' @param info_key (character, default `"taxa_name"`) The column of `info_tbl`
#'  holding the submitted query name to join on.
#' @param col_prefix (character, default `NULL`) Prefix added to every new
#'  column. If `NULL` and a new column collides with an existing `tax_table`
#'  column, `default_prefix` is used (with a warning). If supplied explicitly
#'  and a collision remains, the function aborts.
#' @param default_prefix (character, default `NULL`) Fallback prefix used on a
#'  collision when `col_prefix` is `NULL`. When both are `NULL`, a collision is
#'  a hard error.
#' @param keep_key (logical, default `TRUE`) If `TRUE`, retain the join key in
#'  the result as a `taxa_name` column (overwriting any existing one); if
#'  `FALSE`, drop it.
#'
#' @returns A phyloseq object whose `tax_table` carries the new columns, with
#'  the original taxa order and `taxa_names()` preserved.
#'
#' @author Adrien Taudiere
#' @keywords internal
#' @seealso [taxnames_from_rank()], [taxonomic_rank_to_taxnames()]
augment_tax_table <- function(
  physeq,
  info_tbl,
  taxonomic_rank,
  info_key = "taxa_name",
  col_prefix = NULL,
  default_prefix = NULL,
  keep_key = TRUE
) {
  if (!info_key %in% colnames(info_tbl)) {
    cli::cli_abort(
      "{.arg info_key} = {.val {info_key}} is not a column of {.arg info_tbl}."
    )
  }

  new_cols <- setdiff(colnames(info_tbl), info_key)

  # Collision handling on the (possibly prefixed) new column names.
  existing_cols <- colnames(physeq@tax_table)
  common_cols <- intersect(paste0(col_prefix, new_cols), existing_cols)

  if (length(common_cols) > 0) {
    if (!is.null(col_prefix)) {
      cli::cli_abort(c(
        "Columns {.val {common_cols}} already exist in the {.field tax_table} even with {.arg col_prefix} = {.val {col_prefix}}.",
        "i" = "Choose a different {.arg col_prefix}."
      ))
    } else if (!is.null(default_prefix)) {
      cli::cli_warn(c(
        "Column name{?s} already exist in tax_table: {.val {common_cols}}",
        "i" = "Adding prefix {.val {default_prefix}} to avoid conflicts."
      ))
      col_prefix <- default_prefix
    } else {
      cli::cli_abort(c(
        "New column{?s} {.val {common_cols}} already exist in the {.field tax_table}.",
        "i" = "Pass {.arg col_prefix} to disambiguate."
      ))
    }
  }

  if (!is.null(col_prefix)) {
    info_tbl <- info_tbl |>
      dplyr::rename_with(
        ~ paste0(col_prefix, .x),
        .cols = -dplyr::all_of(info_key)
      )
  }

  # Build the phyloseq-side join key with the same cleanup as the query side.
  tax_tab <- as.data.frame(unclass(physeq@tax_table), stringsAsFactors = FALSE)
  tax_tab[["taxa_name"]] <- NULL
  tax_tab$.taxa_key <- taxnames_from_rank(
    physeq@tax_table,
    taxonomic_rank,
    clean = TRUE
  )

  join_by <- info_key
  names(join_by) <- ".taxa_key"
  merged <- dplyr::left_join(
    tax_tab,
    info_tbl,
    by = join_by,
    relationship = "many-to-one"
  )

  if (keep_key) {
    merged$taxa_name <- merged$.taxa_key
  }
  merged$.taxa_key <- NULL

  new_physeq <- physeq
  new_physeq@tax_table <- phyloseq::tax_table(as.matrix(merged))
  rownames(new_physeq@tax_table) <- phyloseq::taxa_names(physeq)

  new_physeq
}
