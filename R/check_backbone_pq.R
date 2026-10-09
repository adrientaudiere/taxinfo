#' Check that taxa were verified against the backbone queried downstream
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' [gna_verifier_pq()] records, for each taxon, the Global Names Architecture
#' (GNA) data source of its best match, in columns named after the column they
#' document: `currentCanonicalSimple_dataSourceId` and
#' `currentCanonicalSimple_dataSourceTitleShort` (prefixed like the other
#' columns when `col_prefix` is used, e.g.
#' `taxref_currentCanonicalSimple_dataSourceId`). This function compares these
#' data sources with the backbone queried by a downstream function and emits
#' an informative message when they differ. Mixing two sources of taxonomic
#' truth (e.g. names verified against TAXREF then queried in the GBIF
#' backbone) silently lowers match rates, because each backbone has its own
#' synonymy and spelling.
#'
#' By default the expected backbone is the GBIF checklist used by the GBIF
#' functions of taxinfo (see the `taxinfo.gbif_checklist` option in
#' [gbif_key_to_col()]): the GBIF backbone (GNA data source `11`) or the
#' Catalogue of Life Extended Release (GNA data source `13`, Catalogue of Life
#' XR; names verified against the base Catalogue of Life, GNA data source `1`,
#' are also accepted). The GBIF functions ([tax_gbif_occur_pq()],
#' [tax_gbif_alt()], [tax_iucn_code_pq()], [tax_photos_pq()],
#' [tax_occur_check_pq()], [tax_ecoregion_occur_pq()]) call this check
#' automatically when a phyloseq object is provided. So do
#' [tax_retroblast_pq()] (expected data source: NCBI, `4`),
#' [tax_get_wk_info_pq()] and [tax_photos_pq()] with `source = "wikitaxa"`
#' (expected data source: Wikidata, `207`). See
#' `vignette("data-sources")` for the data source matching each function.
#'
#' @param physeq (phyloseq, required) A phyloseq object, typically the output
#'   of [gna_verifier_pq()].
#' @param expected_source (integer vector, default `NULL`) GNA data source
#'   id(s) of the backbone queried downstream (e.g. `11` for the GBIF backbone,
#'   `4` for NCBI, `210` for TaxRef). If `NULL`, it is derived from
#'   `checklist`.
#' @param checklist (character, default `NULL`) GBIF checklist spec used when
#'   `expected_source` is `NULL`: `"backbone"`, `"colxr"` or `NULL` (use the
#'   `taxinfo.gbif_checklist` option). A custom checklist UUID cannot be mapped
#'   to a GNA data source, so the check is skipped.
#' @param taxonomic_rank (character, default `"currentCanonicalSimple"`) The
#'   `tax_table` column(s) used as taxonomic names by the downstream function.
#'   The data source column is `<taxonomic_rank>_dataSourceId`. For the other
#'   [gna_verifier_pq()] output columns (e.g. `"genusEpithet"` or
#'   `"taxref_genusSpeciesEpithet"`), the source column of the
#'   `currentCanonicalSimple` column with the same prefix is used. When no
#'   source column applies (e.g. `c("Genus", "Species")`), nothing is checked.
#' @param source_col (character, default `NULL`) Name of the data source
#'   column, to bypass the lookup from `taxonomic_rank`.
#' @param verbose (logical, default `TRUE`) If `TRUE`, prompt the messages.
#'
#' @returns Invisibly, a tibble with one row per data source: `dataSourceId`,
#'   `dataSourceTitleShort`, `n_taxa` and `expected` (logical, `NA` for
#'   unmatched taxa), or `NULL` when no data source column is found.
#' @export
#' @author Adrien Taudiere
#' @seealso [gna_verifier_pq()], [gbif_key_to_col()]
#' @examples
#' \dontrun{
#' data_fungi_mini_taxref <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
#' # Names verified against TAXREF but queried in the GBIF backbone
#' check_backbone_pq(data_fungi_mini_taxref, checklist = "backbone")
#'
#' # Expected backbone given explicitly
#' check_backbone_pq(data_fungi_mini_taxref, expected_source = 210)
#'
#' # Two verifications kept side by side, each with its own data source
#' data_fungi_mini_2 <- data_fungi_mini_taxref |>
#'   gna_verifier_pq(data_sources = 11, col_prefix = "gbif_")
#' check_backbone_pq(
#'   data_fungi_mini_2,
#'   checklist = "backbone",
#'   taxonomic_rank = "gbif_currentCanonicalSimple"
#' )
#' }
check_backbone_pq <- function(
  physeq,
  expected_source = NULL,
  checklist = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  source_col = NULL,
  verbose = TRUE
) {
  if (!inherits(physeq, "phyloseq") || is.null(physeq@tax_table)) {
    cli::cli_abort("{.arg physeq} must be a phyloseq object with a tax_table.")
  }
  if (!is.null(source_col) && !source_col %in% colnames(physeq@tax_table)) {
    cli::cli_abort(
      "{.arg source_col} = {.val {source_col}} is not a column of the tax_table."
    )
  }
  if (is.null(expected_source)) {
    expected_source <- gna_source_of_checklist(checklist)
    if (anyNA(expected_source)) {
      if (verbose) {
        cli::cli_inform(c(
          "i" = "The GBIF checklist cannot be mapped to a GNA data source; set {.arg expected_source}."
        ))
      }
      return(invisible(NULL))
    }
  }

  res <- backbone_source_table(
    physeq,
    expected_source = expected_source,
    taxonomic_rank = taxonomic_rank,
    source_col = source_col
  )
  if (is.null(res)) {
    if (verbose) {
      cli::cli_inform(c(
        "i" = "No GNA data source column found for {.val {taxonomic_rank}} in the tax_table.",
        "i" = "Run {.fn gna_verifier_pq} (with {.code data_source_col = TRUE}) to record it."
      ))
    }
    return(invisible(NULL))
  }
  if (verbose) {
    inform_backbone_mismatch(res, expected_source)
  }
  invisible(res)
}

#' GNA data source ids matching a GBIF checklist
#'
#' @param checklist Checklist spec, see [gbif_checklist_key()].
#' @returns `11L` (GBIF backbone), `c(13L, 1L)` (Catalogue of Life XR, then
#'   the base Catalogue of Life whose names it includes) or `NA_integer_` for
#'   a custom checklist.
#' @noRd
gna_source_of_checklist <- function(checklist = NULL) {
  key <- gbif_checklist_key(checklist)
  if (identical(key, GBIF_CHECKLIST_BACKBONE)) {
    11L
  } else if (identical(key, GBIF_CHECKLIST_COLXR)) {
    c(13L, 1L)
  } else {
    NA_integer_
  }
}

#' Data source column documenting a taxonomic rank column
#'
#' @param taxonomic_rank The `tax_table` column(s) used as taxonomic names.
#' @param tax_cols The column names of the `tax_table`.
#' @returns The name of the data source column, or `NA` when none applies.
#' @noRd
source_col_of_rank <- function(taxonomic_rank, tax_cols) {
  direct <- paste0(taxonomic_rank, "_dataSourceId")
  if (any(direct %in% tax_cols)) {
    return(direct[direct %in% tax_cols][1])
  }
  # Sibling outputs of gna_verifier_pq() share the source of the
  # currentCanonicalSimple column with the same prefix
  gna_cols <- "(genusSpeciesEpithet|genusEpithet|specificEpithet|currentName)$"
  gna_rank <- grep(gna_cols, taxonomic_rank, value = TRUE)[1]
  if (is.na(gna_rank)) {
    return(NA_character_)
  }
  candidate <- paste0(
    sub(gna_cols, "", gna_rank),
    "currentCanonicalSimple_dataSourceId"
  )
  if (candidate %in% tax_cols) {
    candidate
  } else {
    NA_character_
  }
}

#' Count the taxa of each GNA data source
#'
#' @inheritParams check_backbone_pq
#' @returns A tibble (see [check_backbone_pq()]) or `NULL` when no data
#'   source column applies.
#' @noRd
backbone_source_table <- function(
  physeq,
  expected_source,
  taxonomic_rank = "currentCanonicalSimple",
  source_col = NULL
) {
  tax_cols <- colnames(physeq@tax_table)
  if (is.null(source_col)) {
    source_col <- source_col_of_rank(taxonomic_rank, tax_cols)
  }
  if (is.na(source_col)) {
    return(NULL)
  }

  title_col <- sub("dataSourceId$", "dataSourceTitleShort", source_col)
  tax_tab <- as.data.frame(unclass(physeq@tax_table))
  tibble(
    dataSourceId = as.character(tax_tab[[source_col]]),
    dataSourceTitleShort = if (title_col %in% tax_cols) {
      as.character(tax_tab[[title_col]])
    } else {
      NA_character_
    }
  ) |>
    count(
      across(all_of(c("dataSourceId", "dataSourceTitleShort"))),
      name = "n_taxa"
    ) |>
    mutate(
      expected = if_else(
        is.na(.data$dataSourceId),
        NA,
        .data$dataSourceId %in% as.character(expected_source)
      )
    )
}

#' Message listing the taxa verified against an unexpected data source
#'
#' @param res Output of `backbone_source_table()`.
#' @param expected_source GNA data source id(s) of the queried backbone.
#' @returns Invisibly `NULL`, called for its message side-effect.
#' @noRd
inform_backbone_mismatch <- function(res, expected_source) {
  mismatch <- res[!is.na(res$expected) & !res$expected, , drop = FALSE]
  if (nrow(mismatch) == 0) {
    return(invisible(NULL))
  }
  sources <- paste0(
    ifelse(
      is.na(mismatch$dataSourceTitleShort),
      "GNA data source",
      mismatch$dataSourceTitleShort
    ),
    " (id ",
    mismatch$dataSourceId,
    "): ",
    mismatch$n_taxa,
    " taxa"
  )
  names(sources) <- rep("*", length(sources))
  cli::cli_inform(c(
    "i" = "Some taxa were verified with {.fn gna_verifier_pq} against another backbone than the one queried here (GNA {cli::qty(length(expected_source))}data source{?s} {.val {expected_source}}):",
    sources,
    "i" = "Aligning the names on the queried backbone improves match rates, e.g. {.code gna_verifier_pq(data_sources = {expected_source[1]})}."
  ))
  invisible(NULL)
}

#' Inform about a backbone mismatch inside a function querying a database
#'
#' Best-effort and silent on any error, so it never blocks the caller.
#'
#' @param physeq A phyloseq object or `NULL` (then nothing is done).
#' @param expected_source GNA data source id(s) matching the queried database
#'   (e.g. `4` for NCBI, `207` for Wikidata).
#' @param taxonomic_rank The `tax_table` column(s) used as taxonomic names.
#' @returns Invisibly `NULL`.
#' @noRd
check_source_backbone <- function(
  physeq,
  expected_source,
  taxonomic_rank = "currentCanonicalSimple"
) {
  if (is.null(physeq) || anyNA(expected_source)) {
    return(invisible(NULL))
  }
  tryCatch(
    {
      res <- backbone_source_table(
        physeq,
        expected_source = expected_source,
        taxonomic_rank = taxonomic_rank
      )
      if (!is.null(res)) {
        inform_backbone_mismatch(res, expected_source)
      }
    },
    error = function(e) NULL
  )
  invisible(NULL)
}

#' Inform about a GNA / GBIF backbone mismatch in the GBIF functions
#'
#' @inheritParams check_source_backbone
#' @param checklist Checklist spec, see [gbif_checklist_key()].
#' @returns Invisibly `NULL`.
#' @noRd
check_gbif_backbone <- function(
  physeq,
  checklist = NULL,
  taxonomic_rank = "currentCanonicalSimple"
) {
  if (is.null(physeq)) {
    return(invisible(NULL))
  }
  expected_source <- tryCatch(
    gna_source_of_checklist(checklist),
    error = function(e) NA_integer_
  )
  check_source_backbone(physeq, expected_source, taxonomic_rank)
}
