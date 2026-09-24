#' GBIF checklist compatibility layer for rgbif 3.9.0 (COL Extended Release)
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Converts legacy GBIF Backbone numeric taxon keys (e.g. `5231190`,
#' *Passer domesticus*) to Catalogue of Life Extended Release ("COL XR")
#' alpha-numeric keys (e.g. `"4DXXM"`), the identifiers returned by `rgbif`
#' 3.9.0 and later by default. Each key is matched by GBIF with
#' `rgbif::gbif_to_col()`; the key of the matched COL XR usage is returned.
#' A backbone synonym is therefore converted to its accepted COL XR name
#' (e.g. `5451774`, *Agaricus muscarius*, gives `"5TYZ9"`, *Amanita
#' muscaria*).
#'
#' @section GBIF checklist migration (rgbif 3.9.0):
#' Since `rgbif` 3.9.0 the default GBIF taxonomy is the COL Extended Release
#' (checklistKey `"7ddf754f-d193-4cc9-b351-99906754a03b"`) instead of the
#' legacy GBIF Backbone (`"d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"`), and taxon
#' keys become alpha-numeric. taxinfo routes every key-resolving call site
#' through a pinning wrapper so its behaviour does not silently change across
#' rgbif versions (the `checklistKey` pin is only sent when it differs from
#' the checklist the installed rgbif uses by default). The checklist used by
#' the `tax_*` functions is controlled
#' by:
#'
#' - the `taxinfo.gbif_checklist` option (`"colxr"` or `"backbone"`), e.g.
#'   `options(taxinfo.gbif_checklist = "backbone")`. When the option is
#'   unset, taxinfo follows the checklist your installed rgbif uses by
#'   default (the GBIF Backbone before rgbif 3.9.0, COL XR from 3.9.0 on)
#'   and sends no `checklistKey`, so requests stay identical to plain rgbif
#'   calls;
#' - the `checklist` argument of the functions whose downstream GBIF calls
#'   require legacy backbone keys ([tax_iucn_code_pq()] and
#'   [tax_photos_pq()]: `rgbif::name_usage()` is a GBIF-Backbone API and
#'   cannot resolve COL XR keys), which default to `"backbone"`.
#'
#' GBIF occurrence queries interpret taxon keys in the installed rgbif's
#' default checklist, so taxinfo sends the `checklistKey` of the resolved keys
#' (numeric keys: GBIF Backbone, alpha-numeric keys: COL XR) whenever it
#' differs from that default. Predicate downloads of COL XR keys therefore
#' require rgbif >= 3.9.0, and `method = "download_sql"` re-matches COL XR
#' taxa in the GBIF Backbone because the SQL `taxonkey` / `specieskey`
#' columns only hold backbone keys.
#'
#' Use [gbif_key_to_col()] to migrate stored numeric keys to COL XR keys.
#'
#' @param keys A vector of GBIF Backbone numeric taxon keys (numeric or
#'  character), or a data frame with a `usageKey` column of such keys (e.g.
#'  the output of [rgbif::name_backbone_checklist()] with rgbif < 3.9.0).
#'  `NA` keys are allowed.
#'
#' @returns For a vector, a character vector of COL XR keys with the length
#'  and order of `keys` (`NA` for `NA` keys and for keys without a COL XR
#'  match). For a data frame, the input data frame with an added `col_key`
#'  column. Requires `rgbif` >= 3.9.0 (`gbif_to_col()`).
#'
#' @author Adrien Taudiere
#' @seealso [tax_gbif_occur_coords()], [rgbif::name_backbone_checklist()]
#' @examples
#' \dontrun{
#' # Migrate stored numeric keys to COL XR alpha-numeric keys
#' gbif_key_to_col(c(5231190, 5451774))
#' #> [1] "4DXXM" "5TYZ9"
#'
#' # Add a `col_key` column to a table of backbone keys
#' gbif_key_to_col(data.frame(usageKey = c(5231190, NA, 5451774)))
#' }
#' @export
gbif_key_to_col <- function(keys) {
  if (is.data.frame(keys)) {
    if (!"usageKey" %in% names(keys)) {
      cli::cli_abort("{.arg keys} must have a {.field usageKey} column.")
    }
    keys$col_key <- gbif_key_to_col(keys$usageKey)
    return(keys)
  }
  if (gbif_key_checklist(keys) != "backbone") {
    cli::cli_abort(c(
      "{.arg keys} must be GBIF Backbone numeric keys.",
      "i" = "Alpha-numeric keys (e.g. {.val {keys[!is.na(keys)][1]}}) already come from another checklist such as COL XR."
    ))
  }

  if (!"gbif_to_col" %in% getNamespaceExports("rgbif")) {
    cli::cli_abort(c(
      "{.fn rgbif::gbif_to_col} is not available in the installed {.pkg rgbif}.",
      "i" = "COL XR key conversion requires {.pkg rgbif} >= 3.9.0; upgrade with {.code install.packages(\"rgbif\")}."
    ))
  }

  key_chr <- gbif_key_chr(keys)
  query <- unique(key_chr[!is.na(key_chr)])
  if (length(query) == 0) {
    return(rep(NA_character_, length(keys)))
  }
  gbif_to_col <- getExportedValue("rgbif", "gbif_to_col")
  res <- gbif_to_col(query, checklistKey = GBIF_CHECKLIST_COLXR)
  if (length(query) == 1) {
    res <- list(res)
  }
  col_keys <- vapply(
    res,
    function(x) {
      if (is.null(x$usage$key)) {
        NA_character_
      } else {
        x$usage$key
      }
    },
    character(1)
  )
  unname(col_keys[match(key_chr, query)])
}

#' GBIF taxon keys as character strings
#'
#' `as.character()` writes round numeric keys in scientific notation
#' (`as.character(2e6)` is `"2e+06"`), so numeric and character keys would
#' not compare equal. Numeric keys are written in full instead.
#' @param keys GBIF taxon keys (numeric or character).
#' @returns A character vector (`NA` for `NA` keys).
#' @author Adrien Taudiere
#' @keywords internal
gbif_key_chr <- function(keys) {
  out <- if (is.numeric(keys)) {
    sprintf("%.0f", keys)
  } else {
    as.character(keys)
  }
  out[is.na(keys)] <- NA_character_
  out
}

# Checklist identifiers (GBIF dataset UUIDs) -------------------------------

#' COL Extended Release checklistKey (rgbif >= 3.9.0 default)
#' @noRd
GBIF_CHECKLIST_COLXR <- "7ddf754f-d193-4cc9-b351-99906754a03b"

#' Legacy GBIF Backbone Taxonomy checklistKey
#' @noRd
GBIF_CHECKLIST_BACKBONE <- "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"

#' The checklist the installed rgbif uses by default
#'
#' `"colxr"` from rgbif 3.9.0 on (COL Extended Release), `"backbone"` before
#' (legacy GBIF Backbone Taxonomy).
#' @author Adrien Taudiere
#' @keywords internal
gbif_ambient_checklist <- function() {
  if (utils::packageVersion("rgbif") >= "3.9.0") {
    "colxr"
  } else {
    "backbone"
  }
}

#' Resolve a checklist spec to a GBIF checklistKey
#'
#' @param checklist `NULL` (use the `taxinfo.gbif_checklist` option, falling
#'  back to [gbif_ambient_checklist()]), `"colxr"`, `"backbone"`, or a raw
#'  GBIF dataset UUID (passed through unchanged).
#' @returns A GBIF checklistKey string.
#' @author Adrien Taudiere
#' @keywords internal
gbif_checklist_key <- function(checklist = NULL) {
  if (is.null(checklist)) {
    checklist <- getOption("taxinfo.gbif_checklist", gbif_ambient_checklist())
  }
  if (
    is.character(checklist) &&
      length(checklist) == 1 &&
      grepl("^[0-9a-f]{8}-[0-9a-f]{4}-", checklist)
  ) {
    return(checklist)
  }
  match.arg(checklist, c("colxr", "backbone")) |>
    switch(
      colxr = GBIF_CHECKLIST_COLXR,
      backbone = GBIF_CHECKLIST_BACKBONE
    )
}

#' Checklist-pinned [rgbif::name_backbone_checklist()]
#'
#' Same interface as [rgbif::name_backbone_checklist()], but the target
#' checklist can be pinned explicitly (see [gbif_key_to_col()]) so that
#' resolved keys do not depend on the installed rgbif version. The
#' `checklistKey` parameter is only sent when it differs from the checklist
#' the installed rgbif uses by default: requests stay identical to plain
#' rgbif calls unless a pin is actually required. All taxinfo call sites
#' resolve GBIF names through this wrapper.
#' @param name_data (character vector or data frame) Names to match.
#' @param checklist Checklist spec, see [gbif_checklist_key()].
#' @param ... Passed on to [rgbif::name_backbone_checklist()].
#' @author Adrien Taudiere
#' @keywords internal
gbif_backbone_checklist <- function(name_data, checklist = NULL, ...) {
  key <- gbif_checklist_key(checklist)
  pinned <- !identical(key, gbif_checklist_key(gbif_ambient_checklist()))
  if (!pinned) {
    return(rgbif::name_backbone_checklist(name_data, ...))
  }
  if ("checklistKey" %in% names(formals(rgbif::name_backbone_checklist))) {
    rgbif::name_backbone_checklist(name_data, checklistKey = key, ...)
  } else {
    cli::cli_warn(
      "The installed {.pkg rgbif} has no {.arg checklistKey} argument; matching against its default checklist."
    )
    rgbif::name_backbone_checklist(name_data, ...)
  }
}

#' Resolve names to GBIF taxa (EXACT or HIGHERRANK matches)
#'
#' Runs [gbif_backbone_checklist()] and keeps the `EXACT` / `HIGHERRANK`
#' matches, one row per (`usageKey`, `canonicalName`, `verbatim_name`). When
#' no name matches, GBIF returns neither a `usageKey` nor a `canonicalName`
#' column; the result is then a zero-row tibble with these three columns, so
#' callers can rely on them, and a warning alert is shown (callers then skip
#' every GBIF occurrence call and return their usual, empty output).
#' @param taxnames (character) Names to match.
#' @param checklist Checklist spec, see [gbif_checklist_key()].
#' @returns A tibble with columns `usageKey`, `canonicalName` and
#'  `verbatim_name`.
#' @author Adrien Taudiere
#' @keywords internal
gbif_match_taxa <- function(taxnames, checklist = NULL) {
  res <- gbif_backbone_checklist(taxnames, checklist = checklist)
  if (all(c("usageKey", "canonicalName") %in% names(res))) {
    res <- res |>
      dplyr::filter(.data$matchType %in% c("EXACT", "HIGHERRANK")) |>
      dplyr::distinct(.data$usageKey, .data$canonicalName, .data$verbatim_name)
  } else {
    res <- tibble::tibble(
      usageKey = character(),
      canonicalName = character(),
      verbatim_name = character()
    )
  }
  if (nrow(res) == 0) {
    cli::cli_alert_warning(
      "None of the {.val {length(taxnames)}} queried name{?s} matched a GBIF taxon."
    )
  }
  res
}

#' Checklist-pinned [rgbif::name_backbone()]
#'
#' Same interface as [rgbif::name_backbone()], with the checklist pinned as in
#' [gbif_backbone_checklist()].
#' @param name (character) Name to match.
#' @param checklist Checklist spec, see [gbif_checklist_key()].
#' @param ... Passed on to [rgbif::name_backbone()].
#' @author Adrien Taudiere
#' @keywords internal
gbif_backbone <- function(name, checklist = NULL, ...) {
  key <- gbif_checklist_key(checklist)
  pinned <- !identical(key, gbif_checklist_key(gbif_ambient_checklist()))
  if (!pinned) {
    return(rgbif::name_backbone(name, ...))
  }
  if ("checklistKey" %in% names(formals(rgbif::name_backbone))) {
    rgbif::name_backbone(name, checklistKey = key, ...)
  } else {
    cli::cli_warn(
      "The installed {.pkg rgbif} has no {.arg checklistKey} argument; matching against its default checklist."
    )
    rgbif::name_backbone(name, ...)
  }
}

#' Backbone-safe [rgbif::name_usage()]
#'
#' `rgbif::name_usage()` only understands legacy GBIF Backbone numeric keys
#' (it is deprecated for COL XR keys in rgbif 3.9.0 and cannot resolve them,
#' even with an explicit `datasetKey`). This wrapper pins the backbone
#' `datasetKey` and rejects alpha-numeric keys with an actionable error.
#' Call sites using it ([tax_iucn_code_pq()], [tax_photos_pq()]) therefore
#' resolve names with `checklist = "backbone"`.
#' @param key A GBIF Backbone numeric usage key.
#' @param data (character) Passed to [rgbif::name_usage()].
#' @param ... Passed on to [rgbif::name_usage()].
#' @author Adrien Taudiere
#' @keywords internal
gbif_name_usage <- function(key, data = "all", ...) {
  if (!all(grepl("^[0-9]+$", as.character(key)))) {
    cli::cli_abort(c(
      "{.fn rgbif::name_usage} only understands legacy GBIF Backbone numeric keys.",
      "i" = "Resolve names with {.code checklist = \"backbone\"} (or {.code options(taxinfo.gbif_checklist = \"backbone\")}) before calling this API.",
      "i" = "For COL XR keys use {.fn rcol::col_usage} instead."
    ))
  }
  # The datasetKey pin silences the rgbif 3.9.0 deprecation warning; it is
  # only sent there, so requests stay identical to plain rgbif calls before.
  if (
    utils::packageVersion("rgbif") >= "3.9.0" &&
      "datasetKey" %in% names(formals(rgbif::name_usage))
  ) {
    rgbif::name_usage(
      key,
      data = data,
      datasetKey = GBIF_CHECKLIST_BACKBONE,
      ...
    )
  } else {
    rgbif::name_usage(key, data = data, ...)
  }
}

# Occurrence queries ---------------------------------------------------------

#' Checklist a set of GBIF taxon keys belongs to
#'
#' Legacy GBIF Backbone keys are numeric (`5231190`), COL XR keys are
#' alpha-numeric (`"Q2M4"`): the same rule rgbif 3.9.0 uses to route its
#' occurrence queries.
#' @param keys GBIF taxon keys.
#' @returns `"backbone"` or `"colxr"`.
#' @author Adrien Taudiere
#' @keywords internal
gbif_key_checklist <- function(keys) {
  keys <- keys[!is.na(keys)]
  if (is.numeric(keys) || all(grepl("^[0-9]+$", keys))) {
    "backbone"
  } else {
    "colxr"
  }
}

#' `checklistKey` argument for a GBIF occurrence call
#'
#' GBIF occurrence endpoints interpret `taxonKey` in the default checklist of
#' the installed rgbif (GBIF Backbone before 3.9.0, COL XR from 3.9.0 on), so
#' a key resolved in the other checklist silently matches no occurrence. This
#' helper returns the `checklistKey` matching the checklist of `keys` (see
#' [gbif_key_checklist()]) when it differs from that default, and an empty
#' list otherwise so that requests stay identical to plain rgbif calls.
#' @param keys GBIF taxon keys sent in the occurrence call.
#' @param fun The rgbif function receiving the argument (used to check that
#'  it accepts `checklistKey`).
#' @returns An empty list or `list(checklistKey = <uuid>)`, to splice into the
#'  arguments of `fun`.
#' @author Adrien Taudiere
#' @keywords internal
gbif_occ_checklist_args <- function(keys, fun = rgbif::occ_search) {
  checklist <- gbif_key_checklist(keys)
  if (identical(checklist, gbif_ambient_checklist())) {
    return(list())
  }
  if (!"checklistKey" %in% names(formals(fun))) {
    cli::cli_abort(c(
      "The installed {.pkg rgbif} cannot send {.val {checklist}} taxon keys to this GBIF occurrence API.",
      "i" = "Upgrade {.pkg rgbif} to >= 3.9.0, or resolve names in the GBIF Backbone with {.code options(taxinfo.gbif_checklist = \"backbone\")}."
    ))
  }
  list(checklistKey = gbif_checklist_key(checklist))
}

#' Checklist-aware GBIF occurrence count
#'
#' [rgbif::occ_count()] does not forward a `checklistKey` to GBIF. When
#' `taxon_key` belongs to the default checklist of the installed rgbif the
#' call is a plain [rgbif::occ_count()]; otherwise the count is read from an
#' equivalent [rgbif::occ_search()] call (`limit = 0`,
#' `occurrenceStatus = "PRESENT"`) carrying the `checklistKey` of
#' `taxon_key` (see [gbif_occ_checklist_args()]).
#' @param taxon_key A GBIF taxon key.
#' @param ... Other occurrence filters (e.g. `hasCoordinate = TRUE`).
#' @returns The number of occurrence records (numeric).
#' @author Adrien Taudiere
#' @keywords internal
gbif_occ_count <- function(taxon_key, ...) {
  checklist_args <- gbif_occ_checklist_args(taxon_key)
  if (length(checklist_args) == 0) {
    return(rgbif::occ_count(taxonKey = taxon_key, ...))
  }
  args <- c(
    list(
      taxonKey = taxon_key,
      occurrenceStatus = "PRESENT",
      limit = 0,
      ...
    ),
    checklist_args
  )
  do.call(rgbif::occ_search, args)$meta$count
}

#' Re-express resolved GBIF taxa with GBIF Backbone keys
#'
#' GBIF SQL downloads filter on the literal `taxonkey` / `specieskey`
#' occurrence columns, which always hold GBIF Backbone numeric keys. Taxa
#' resolved in another checklist (COL XR) are therefore re-matched by name in
#' the GBIF Backbone; taxa without a backbone match are dropped with a
#' warning.
#' @param gbif_taxa (tibble) Resolved taxa with `usageKey` and
#'  `verbatim_name`.
#' @param verbose (logical) Report the re-matching and the dropped taxa.
#' @returns `gbif_taxa` with GBIF Backbone `usageKey`s (unchanged when its
#'  keys already are backbone keys).
#' @author Adrien Taudiere
#' @keywords internal
gbif_taxa_to_backbone <- function(gbif_taxa, verbose = TRUE) {
  if (gbif_key_checklist(gbif_taxa$usageKey) == "backbone") {
    return(gbif_taxa)
  }
  if (verbose) {
    cli::cli_alert_info(
      "GBIF SQL downloads only know GBIF Backbone keys: re-matching {.val {nrow(gbif_taxa)}} taxa in the GBIF Backbone."
    )
  }
  bb <- gbif_backbone_checklist(
    unique(gbif_taxa$verbatim_name),
    checklist = "backbone"
  )
  bb_keys <- if ("usageKey" %in% names(bb)) {
    bb$usageKey
  } else {
    rep(NA, nrow(bb))
  }
  bb_keys[!bb$matchType %in% c("EXACT", "HIGHERRANK")] <- NA
  new_keys <- bb_keys[match(gbif_taxa$verbatim_name, bb$verbatim_name)]

  dropped <- gbif_taxa$verbatim_name[is.na(new_keys)]
  if (length(dropped) > 0) {
    cli::cli_alert_warning(
      "No GBIF Backbone match, skipped by the SQL download: {.emph {dropped}}"
    )
  }
  out <- gbif_taxa[!is.na(new_keys), , drop = FALSE]
  out$usageKey <- new_keys[!is.na(new_keys)]
  out
}
