#' Add FunEndo endophytic-fungi information to a phyloseq object
#'
#' @description
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Augments the `tax_table` slot of a phyloseq object (or a vector of
#' taxonomic names) with summary information from the FunEndo global fungal
#' endophyte repository (Moghaddam 2026,
#' <https://doi.org/10.65390/fdiv.2026.136016>). FunEndo is a *long* table:
#' one row per (reference, accession) endophyte record, so a taxon usually
#' matches many rows. This function aggregates the matching records into
#' per-taxon summary columns: record/reference/accession counts and the
#' distinct values (collapsed to a single string) of the organ colonised,
#' study type, reference type, country and host genus.
#'
#' Matching is done species-first with a genus fallback (as in
#' [tax_metatraits_pq()]): a taxonomic name containing an epithet is matched
#' against the FunEndo `Species` column (binomials of the form
#' `"Genus epithet"`), and names without a species match (or genus-only
#' names) fall back to all the FunEndo records of their genus. The column
#' `match_level` records where the aggregation happened (`"species"`,
#' `"genus"` or `NA` for no match).
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames`
#'  must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default `"currentCanonicalSimple"`) The
#'  column(s) of the `tax_table` slot of the phyloseq object used to build
#'  the query names. See [tax_info_pq()].
#' @param file_name (Character, default `NULL`) Path to a CSV file following
#'  the FunEndo column schema. If `NULL`, the FunEndo v0.1 repository bundled
#'  with taxinfo (`funendo_v0.1.csv.gz`) is used. The smaller
#'  `funendo_mini.csv` file bundled with the package is handy for examples
#'  and quick tests.
#' @param match_level (Character vector, default `c("species", "genus")`).
#'  Matching levels allowed. Their order does not matter: a species match
#'  always wins over the genus fallback. Use `"species"` alone to forbid
#'  genus-level aggregation.
#' @param max_levels (Integer, default `30`) Maximum number of distinct values
#'  kept in each collapsed column (e.g. `countries`). When truncated, the
#'  string ends with `"(+N more)"`.
#' @param collapse (Character, default `" | "`) Separator used to collapse the
#'  distinct values of a multi-valued field into one string.
#' @param col_prefix (Character, default `"fe_"`) Prefix added to every new
#'  column of the `tax_table`.
#' @param add_to_phyloseq (Logical, default `TRUE` when `physeq` is provided,
#'  `FALSE` when `taxnames` is provided) If `TRUE`, return a new phyloseq
#'  object with new columns in the `tax_table` slot. If `FALSE`, return a
#'  tibble of the augmented `tax_table` (physeq input) or of the per-name
#'  summary (taxnames input).
#' @param verbose (Logical, default `TRUE`) If `TRUE`, prompt some messages.
#' @param discard_genus_alone (Logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`) Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (Logical, default `TRUE`) Passed to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns Either an updated phyloseq object (when `add_to_phyloseq = TRUE`)
#'  or a tibble. The added columns are `fe_match_level`, `fe_n_records`,
#'  `fe_n_references`, `fe_n_accessions`, `fe_organs`, `fe_study_types`,
#'  `fe_ref_types`, `fe_countries` and `fe_host_genera`. As everywhere in the
#'  `tax_*_pq` family, all summary columns are character (they live in a
#'  `tax_table` matrix); use `as.numeric()` on the count columns as needed.
#'
#' @details
#' The genus-level aggregation pools **all** the FunEndo records of the genus
#' (species-level records included), so `fe_n_records` at genus level counts
#' every endophyte record attributed to the genus. Records are de-duplicated
#' (exact duplicates removed) before aggregation. `fe_n_references` counts
#' distinct references: records are grouped by their DOI when the `DOI` field
#' contains one (normalised, so `https://doi.org/10.x/y` and
#' `http://dx.doi.org/10.x/y` are the same reference), and by their
#' `Reference` text otherwise (e.g. the many `"Unpublished"` GenBank
#' submissions, each counted once per distinct title). As in the rest of the
#' `tax_*_pq` family, query names are matched verbatim (after `"_"` to `" "`
#' normalisation): run [gna_verifier_pq()] first to obtain
#' `currentCanonicalSimple` names.
#'
#' @author Adrien Taudiere
#' @references
#' Moghaddam, M. S. H. (2026). FunEndo: A Comprehensive Global-Scale Fungal
#' Endophyte Repository. *Fungal Diversity*.
#' \doi{10.65390/fdiv.2026.136016}
#'
#' @seealso [tax_info_pq()], [tax_metatraits_pq()], [fungal_traits_guilds()]
#' @examples
#' \donttest{
#' data(data_fungi_mini)
#'
#' # The bundled mini version keeps the example fast and fully offline
#' fe_mini <- tax_funendo_pq(data_fungi_mini,
#'   taxonomic_rank = "Genus_species",
#'   file_name = system.file("extdata", "funendo_mini.csv",
#'     package = "taxinfo"
#'   ),
#'   add_to_phyloseq = FALSE,
#'   verbose = FALSE
#' )
#' head(fe_mini[, c("taxa_name", "fe_match_level", "fe_n_records", "fe_organs")])
#' }
#'
#' \dontrun{
#' # Full FunEndo repository (~104 000 endophyte records)
#' data(data_fungi_mini)
#' res <- tax_funendo_pq(data_fungi_mini,
#'   taxonomic_rank = "Genus_species",
#'   add_to_phyloseq = TRUE
#' )
#' table(res@tax_table[, "fe_match_level"], useNA = "ifany")
#' table(res@tax_table[, "fe_organs"], useNA = "ifany")
#' }
#' @export
tax_funendo_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  file_name = NULL,
  match_level = c("species", "genus"),
  max_levels = 30,
  collapse = " | ",
  col_prefix = "fe_",
  add_to_phyloseq = NULL,
  verbose = TRUE,
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
  taxnames_vec <- resolved$taxnames
  add_to_phyloseq <- resolved$add_to_phyloseq
  match_level <- match.arg(
    match_level,
    c("species", "genus"),
    several.ok = TRUE
  )

  if (is.null(file_name)) {
    file_name <- system.file(
      "extdata",
      "funendo_v0.1.csv.gz",
      package = "taxinfo"
    )
  }
  if (!nzchar(file_name) || !file.exists(file_name)) {
    cli::cli_abort("The file path {.path {file_name}} does not exist")
  }

  fe <- utils::read.csv(file_name, colClasses = "character")

  required_cols <- c(
    "Species",
    "Genus",
    "Organ",
    "Study_Type",
    "Ref_Type",
    "Reference",
    "DOI",
    "Accession",
    "Country",
    "Host_Genus"
  )
  missing_cols <- setdiff(required_cols, colnames(fe))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Columns missing from {.path {file_name}}: {.val {missing_cols}}"
    )
  }

  fe <- dplyr::distinct(fe)

  fe$fe_species_key <- fe_clean_name(fe$Species)
  fe$fe_genus_key <- fe_clean_name(fe$Genus)
  fe$fe_ref_key <- fe_reference_key(fe$DOI, fe$Reference)

  species_agg <- fe_aggregate(
    fe[!is.na(fe$fe_species_key), , drop = FALSE],
    key_col = "fe_species_key",
    collapse = collapse,
    max_levels = max_levels
  )
  genus_agg <- fe_aggregate(
    fe[!is.na(fe$fe_genus_key), , drop = FALSE],
    key_col = "fe_genus_key",
    collapse = collapse,
    max_levels = max_levels
  )

  # Assemble one summary row per unique query name (species-first, then the
  # genus fallback layer). The `taxa_name` key stays in the *submitted* query
  # namespace so that augment_tax_table() can join it back unchanged.
  keys <- unique(taxnames_vec)
  query <- fe_clean_name(keys)
  keep <- !is.na(query) & !is.na(keys) & keys != ""
  keys <- keys[keep]
  query <- query[keep]
  value_cols <- setdiff(colnames(species_agg), "taxa_name")

  result <- as.data.frame(
    matrix(
      NA_character_,
      nrow = length(keys),
      ncol = length(value_cols),
      dimnames = list(NULL, value_cols)
    ),
    stringsAsFactors = FALSE
  )

  if ("genus" %in% match_level) {
    q_genus <- sub(" .*$", "", query)
    gm <- match(q_genus, genus_agg$taxa_name)
    for (col in value_cols) {
      result[[col]] <- genus_agg[[col]][gm]
    }
  }
  if ("species" %in% match_level) {
    q_species <- ifelse(grepl(" ", query), query, NA_character_)
    sm <- match(q_species, species_agg$taxa_name)
    for (col in value_cols) {
      sval <- species_agg[[col]][sm]
      result[[col]][!is.na(sval)] <- sval[!is.na(sval)]
    }
    match_idx <- !is.na(sm)
  } else {
    match_idx <- rep(FALSE, length(keys))
  }

  match_level_col <- rep(NA_character_, length(keys))
  matched_any <- !is.na(result[["n_records"]])
  match_level_col[matched_any & !match_idx] <- "genus"
  match_level_col[match_idx] <- "species"

  info_tbl <- dplyr::bind_cols(
    tibble::tibble(taxa_name = keys, match_level = match_level_col),
    tibble::as_tibble(result)
  )

  if (verbose) {
    cli::cli_alert_success(
      "Summarized FunEndo records for {.val {sum(matched_any)}}/{.val {length(keys)}} taxonomic name{?s} ({.val {sum(match_level_col == 'species', na.rm = TRUE)}} at species level)."
    )
  }

  if (!is.null(physeq)) {
    new_physeq <- augment_tax_table(
      physeq = physeq,
      info_tbl = info_tbl,
      taxonomic_rank = taxonomic_rank,
      col_prefix = col_prefix
    )
    if (add_to_phyloseq) {
      return(new_physeq)
    } else {
      return(tibble::as_tibble(as.data.frame(new_physeq@tax_table)))
    }
  }

  # taxnames input: no phyloseq to merge back into, so prefix the summary
  # columns directly to keep the same `fe_*` naming as the phyloseq path.
  dplyr::rename_with(
    info_tbl,
    ~ paste0(col_prefix, .x),
    .cols = -dplyr::all_of("taxa_name")
  )
}

# Internal helpers -------------------------------------------------------------

#' Normalise a taxonomic-name vector for FunEndo matching
#'
#' Turns `"Genus_epithet"` style names into `"Genus epithet"` and maps the
#' usual blank sentinels (`""`, `"NA"`) to `NA`. The raw (unnormalised) names
#' are kept on the query side as join keys; this cleaning happens only for
#' matching against the FunEndo columns.
#' @noRd
fe_clean_name <- function(x) {
  x <- trimws(gsub("_", " ", as.character(x)))
  x[is.na(x) | x %in% c("", "NA", "NA NA")] <- NA_character_
  x
}

#' Build the key identifying the reference of each FunEndo record
#'
#' The FunEndo `DOI` column mixes real DOIs written in many forms
#' (`https://doi.org/10.x/y`, `http://dx.doi.org/10.x%2Fy`, `doi.10.x/y`, a
#' trailing `.`, ...) with placeholders such as `"Unpublished"` (about 42% of
#' the records, spread over ~1 800 distinct GenBank submissions), free text
#' and ISBNs. The key is the DOI, extracted and normalised (lower case, no
#' prefix, no trailing punctuation), when the `DOI` field contains one, and
#' the normalised `Reference` text (lower case, collapsed whitespace)
#' otherwise.
#' @noRd
fe_reference_key <- function(doi, reference) {
  doi <- gsub("%2F", "/", doi, ignore.case = TRUE)
  m <- regexpr("10\\.[0-9]{4,9}/[^[:space:]]+", doi)
  doi_key <- rep(NA_character_, length(doi))
  found <- !is.na(m) & m > 0
  doi_key[found] <- regmatches(doi, m)
  # Some fields repeat the DOI as a URL right after it ("10.x/yhttps://...").
  doi_key <- sub("https?:.*$", "", doi_key)
  doi_key <- tolower(sub("[.,;]+$", "", doi_key))

  ref_key <- tolower(trimws(gsub("[[:space:]]+", " ", reference)))
  ref_key[is.na(ref_key) | ref_key %in% c("", "na")] <- NA_character_

  ifelse(is.na(doi_key), ref_key, doi_key)
}

#' Collapse the distinct values of a multi-valued field into one string
#'
#' Values are ordered by decreasing frequency (then alphabetically), at most
#' `max_levels` of them are kept, and truncation is signalled by a trailing
#' `"(+N more)"` marker.
#' @noRd
fe_collapse_unique <- function(x, collapse, max_levels) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  tb <- sort(table(x), decreasing = TRUE)
  vals <- names(tb)
  n_extra <- 0
  if (length(vals) > max_levels) {
    n_extra <- length(vals) - max_levels
    vals <- vals[seq_len(max_levels)]
  }
  out <- paste(vals, collapse = collapse)
  if (n_extra > 0) {
    out <- paste0(out, collapse, "(+", n_extra, " more)")
  }
  out
}

#' Aggregate FunEndo rows per taxonomic key into one summary row per key
#'
#' Returns a tibble with the key column renamed to `taxa_name` and the
#' summary columns `n_records`, `n_references`, `n_accessions`, `organs`,
#' `study_types`, `ref_types`, `countries` and `host_genera`.
#' @noRd
fe_aggregate <- function(df, key_col, collapse, max_levels) {
  df |>
    dplyr::group_by(.data[[key_col]]) |>
    dplyr::summarise(
      n_records = as.character(dplyr::n()),
      n_references = as.character(
        length(unique(.data$fe_ref_key[!is.na(.data$fe_ref_key)]))
      ),
      n_accessions = as.character(
        length(unique(.data$Accession[!is.na(.data$Accession)]))
      ),
      organs = fe_collapse_unique(.data$Organ, collapse, max_levels),
      study_types = fe_collapse_unique(.data$Study_Type, collapse, max_levels),
      ref_types = fe_collapse_unique(.data$Ref_Type, collapse, max_levels),
      countries = fe_collapse_unique(.data$Country, collapse, max_levels),
      host_genera = fe_collapse_unique(.data$Host_Genus, collapse, max_levels),
      .groups = "drop"
    ) |>
    dplyr::rename(taxa_name = dplyr::all_of(key_col))
}
