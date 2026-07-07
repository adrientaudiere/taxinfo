#' Add range-shift information from the BioShifts database to a phyloseq object
#'
#' @description
#'
#' <a href="https://adrientaudiere.github.io/taxinfo/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Query the BioShifts database (a global compilation of documented species
#' range shifts under climate change) through the `BioShiftR` package and
#' attach, for each taxon of a `phyloseq` object, a summary of the observed
#' range-shift rates. Latitudinal (`"LAT"`) and elevational (`"ELE"`) shifts are
#' summarized separately as the mean shift rate across all matching records,
#' together with the number of records found.
#'
#' Species are matched by name between the `taxonomic_rank` column(s) of the
#' `tax_table` and the `sp_name_checked` column returned by
#' `BioShiftR::get_shifts()`. By default the BioShifts names are first
#' harmonized with [gna_verifier_pq()] and the join is performed on the
#' resulting `currentCanonicalSimple` names, so that synonyms and orthographic
#' variants on the BioShifts side are reconciled with the (already GNA-verified)
#' names of the `phyloseq` object. Set `skip_name_verification = TRUE` to skip
#' this correction and match the raw `sp_name_checked` values directly. Matching
#' is always case- and separator-insensitive, so a single `"Genus_species"`
#' column (underscores) matches BioShifts' `"Genus species"` (spaces).
#'
#' @param physeq (required) A \code{\link[phyloseq]{phyloseq-class}} object with
#'   a `tax_table` containing the `taxonomic_rank` column(s).
#' @param taxonomic_rank (character, default `"currentCanonicalSimple"`) The
#'   `tax_table` column(s) holding the taxon name used for matching. May be a
#'   single column (typically the GNA-verified `currentCanonicalSimple`, see
#'   [gna_verifier_pq()], or a `"Genus_species"` binomial column) or a vector of
#'   columns pasted together in order (e.g. `c("Genus", "Species")` when the
#'   genus and the species epithet are stored separately).
#' @param group,eco,continent,type Filters passed to
#'   `BioShiftR::get_shifts()`. `type` is the shift dimension(s) to summarize and
#'   accepts any subset of `c("LAT", "ELE")`. See `?BioShiftR::get_shifts`.
#' @param skip_name_verification (logical, default `FALSE`) If `FALSE` (the
#'   default), the BioShifts `sp_name_checked` names are harmonized with
#'   [gna_verifier_pq()] and matched to the `phyloseq` names on
#'   `currentCanonicalSimple`. If `TRUE`, this correction is skipped and the raw
#'   `sp_name_checked` values are matched directly. Set to `TRUE` for offline
#'   use or when the names are already known to be consistent.
#' @param data_sources (numeric, default `c(1, 12)`) Data sources passed to
#'   [gna_verifier_pq()] when `skip_name_verification = FALSE`.
#' @param shifts_data (optional data.frame) A pre-fetched
#'   `BioShiftR::get_shifts()` result (with columns `sp_name_checked`, `type`
#'   and `calc_rate`). When supplied, the live query is skipped and `group` /
#'   `eco` / `continent` / `type` are only used to select which `type`s to
#'   summarize. Useful for reproducible analyses and tests.
#' @param col_prefix (character, default `"bioshift_"`) Prefix applied to the
#'   added columns (`bioshift_LAT_rate`, `bioshift_ELE_rate`,
#'   `bioshift_n_records`).
#' @param add_to_phyloseq (logical, default `TRUE`) If `TRUE`, return the
#'   phyloseq object with the new columns added to its `tax_table`; if `FALSE`,
#'   return the augmented `tax_table` as a tibble.
#' @param verbose (logical, default `TRUE`) Print a summary message.
#'
#' @return Either an updated `phyloseq` object (when `add_to_phyloseq = TRUE`)
#'   or a tibble of the augmented `tax_table` (when `add_to_phyloseq = FALSE`).
#' @export
#' @author Adrien Taudière
#' @references
#'   Comte, L., Bertrand, R., Diamond, S., Lenoir, J. et al. (2024) Mechanisms,
#'   detection and impacts of species redistributions under climate change.
#'   Nature Reviews Earth & Environment.
#'
#'   Lenoir, J., Bertrand, R., Comte, L. et al. (2020) Species better track
#'   climate warming in the oceans than on land. Nature Ecology & Evolution 4,
#'   1044-1059.
#'
#'   Data are queried through the in-development `BioShiftR` package
#'   (\url{https://bioshifts.github.io/BioShiftR/}), which serves the current
#'   BioShifts release: the original BioShifts merged with the CoRE database
#'   (Rubenstein et al.), amounting to roughly 31,760 range-shift estimates for
#'   about 12,912 species across marine, freshwater and terrestrial ecosystems.
#' @seealso \code{\link{tax_metatraits_pq}}, \code{\link{gna_verifier_pq}}
#' @examples
#' \dontrun{
#' # Live query (needs the BioShiftR package and network access). By default the
#' # BioShifts names are GNA-verified and matched on `currentCanonicalSimple`,
#' # so run `gna_verifier_pq()` on your phyloseq first:
#' data(data_fungi_mini, package = "MiscMetabar")
#' pq <- gna_verifier_pq(data_fungi_mini)
#' pq <- tax_bioshifts_pq(pq, group = "FUNGI", type = "LAT")
#'
#' # `data_fungi_mini` also ships a single `Genus_species` (underscore) column,
#' # which matches BioShifts directly:
#' data_fungi_mini2 <- data_fungi_mini
#' data_fungi_mini2@tax_table[1:3, "Genus_species"] <- c(
#'   "Ramalina_farinacea", "Evernia_prunastri", "Sphaerophorus_fragilis"
#' )
#' pq2 <- tax_bioshifts_pq(data_fungi_mini2, taxonomic_rank = "Genus_species")
#' }
#'
#' # Offline: supply a pre-fetched shifts table and skip name verification.
#' shifts <- data.frame(
#'   sp_name_checked = c("Genusa speciesa", "Genusa speciesa", "Genusb speciesb"),
#'   type = c("LAT", "LAT", "ELE"),
#'   calc_rate = c(1.2, 0.8, -3.5),
#'   stringsAsFactors = FALSE
#' )
#' otu <- matrix(1, nrow = 2, ncol = 1, dimnames = list(c("t1", "t2"), "s1"))
#' tax <- matrix(
#'   c("Genusa", "speciesa", "Genusb", "speciesb"),
#'   nrow = 2, byrow = TRUE, dimnames = list(c("t1", "t2"), c("Genus", "Species"))
#' )
#' pq <- phyloseq::phyloseq(
#'   phyloseq::otu_table(otu, taxa_are_rows = TRUE),
#'   phyloseq::tax_table(tax)
#' )
#' tax_bioshifts_pq(
#'   pq,
#'   taxonomic_rank = c("Genus", "Species"),
#'   shifts_data = shifts,
#'   skip_name_verification = TRUE,
#'   add_to_phyloseq = FALSE
#' )
tax_bioshifts_pq <- function(
  physeq,
  taxonomic_rank = "currentCanonicalSimple",
  group = "All",
  eco = "All",
  continent = "All",
  type = c("LAT", "ELE"),
  skip_name_verification = FALSE,
  data_sources = c(1, 12),
  shifts_data = NULL,
  col_prefix = "bioshift_",
  add_to_phyloseq = TRUE,
  verbose = TRUE
) {
  if (is.null(physeq) || !methods::is(physeq, "phyloseq")) {
    cli::cli_abort("{.arg physeq} must be a {.cls phyloseq} object.")
  }
  if (is.null(physeq@tax_table)) {
    cli::cli_abort("{.arg physeq} has no {.field tax_table} slot.")
  }
  type <- match.arg(type, c("LAT", "ELE"), several.ok = TRUE)

  tax_df <- as.data.frame(physeq@tax_table)
  missing_rank <- setdiff(taxonomic_rank, colnames(tax_df))
  if (length(missing_rank) > 0) {
    cli::cli_abort(c(
      "Column{?s} {.val {missing_rank}} not found in the {.field tax_table}.",
      "i" = "Available ranks: {.val {colnames(tax_df)}}."
    ))
  }

  if (is.null(shifts_data)) {
    if (!requireNamespace("BioShiftR", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg BioShiftR} is required to query the BioShifts database.",
        "i" = "Install it with {.code remotes::install_github(\"bioshifts/BioShiftR\")}, or pass {.arg shifts_data}."
      ))
    }
    shifts_data <- BioShiftR::get_shifts(
      group = group,
      eco = eco,
      continent = continent,
      type = type
    )
  }
  shifts_data <- as.data.frame(shifts_data)

  needed <- c("sp_name_checked", "type", "calc_rate")
  missing_cols <- setdiff(needed, colnames(shifts_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg shifts_data} is missing required column{?s} {.val {missing_cols}}.",
      "i" = "Available columns: {.val {colnames(shifts_data)}}."
    ))
  }

  # Build the per-taxon join key from the requested tax_table column(s).
  tax_key <- bioshift_norm_name(taxnames_from_rank(
    physeq@tax_table,
    taxonomic_rank,
    clean = TRUE
  ))

  # Build the BioShifts-side join key. By default the raw `sp_name_checked`
  # names are harmonized with the GNA Verifier and matched on their
  # `currentCanonicalSimple`; otherwise they are matched as-is.
  if (skip_name_verification) {
    shifts_data$.key <- bioshift_norm_name(shifts_data$sp_name_checked)
  } else {
    uniq_names <- unique(shifts_data$sp_name_checked)
    uniq_names <- uniq_names[!is.na(uniq_names) & trimws(uniq_names) != ""]
    verified <- gna_verifier_pq(
      taxnames = uniq_names,
      data_sources = data_sources,
      add_to_phyloseq = FALSE,
      genus_species_canonical_col = FALSE,
      year_col = FALSE,
      authorship_col = FALSE,
      verbose = verbose
    )
    # Key the map on the (normalized) `submittedName`, the verifier's own
    # within-row echo of the queried name. This avoids relying on the positional
    # `taxa_names_in_phyloseq <- taxnames` alignment (fragile under name
    # slicing), and normalizing neutralizes any re-capitalization.
    canonical_by_name <- stats::setNames(
      verified$currentCanonicalSimple,
      bioshift_norm_name(verified$submittedName)
    )
    shifts_data$.key <- bioshift_norm_name(
      canonical_by_name[bioshift_norm_name(shifts_data$sp_name_checked)]
    )
  }

  result <- data.frame(row.names = taxa_names(physeq))
  for (ty in type) {
    sub <- shifts_data[
      shifts_data$type == ty & !is.na(shifts_data$calc_rate),
      ,
      drop = FALSE
    ]
    if (nrow(sub) > 0) {
      agg <- tapply(sub$calc_rate, sub$.key, mean, na.rm = TRUE)
      result[[paste0(ty, "_rate")]] <- as.numeric(agg[tax_key])
    } else {
      result[[paste0(ty, "_rate")]] <- NA_real_
    }
  }

  key_counts <- table(shifts_data$.key[!is.na(shifts_data$.key)])
  n_records <- as.integer(key_counts[tax_key])
  n_records[is.na(n_records)] <- 0L
  result[["n_records"]] <- n_records

  colnames(result) <- paste0(col_prefix, colnames(result))

  if (verbose) {
    n_matched <- sum(result[[paste0(col_prefix, "n_records")]] > 0)
    cli::cli_inform(
      "Matched BioShifts records for {.val {n_matched}}/{.val {nrow(tax_df)}} taxa."
    )
  }

  new_tax_df <- cbind(tax_df, result)

  if (add_to_phyloseq) {
    # Note: a phyloseq `tax_table` is a character matrix, so the numeric
    # shift columns are stored as character strings on the returned object.
    # Use `add_to_phyloseq = FALSE` to keep them numeric.
    new_physeq <- physeq
    phyloseq::tax_table(new_physeq) <- phyloseq::tax_table(as.matrix(
      new_tax_df
    ))
    new_physeq
  } else {
    tibble::as_tibble(new_tax_df)
  }
}

#' Normalise a taxon name for case- and separator-insensitive matching
#'
#' @param x Character vector.
#' @return Lower-cased, whitespace-collapsed character vector (`NA` on blanks).
#' @keywords internal
#' @noRd
bioshift_norm_name <- function(x) {
  x <- tolower(gsub("[_ ]+", " ", trimws(as.character(x))))
  x[x == ""] <- NA_character_
  x
}
