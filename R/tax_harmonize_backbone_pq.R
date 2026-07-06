# Harmonise higher taxonomic ranks against a trusted backbone

################################################################################
#' Harmonise higher taxonomic ranks from a trusted backbone
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Rebuild the **parent (higher) taxonomic ranks** of a phyloseq object from a
#' single trusted taxonomic backbone, starting from a chosen *anchor* rank (or,
#' per taxon, the deepest assigned rank). The anchor value (e.g. a `Genus` name)
#' is looked up in the backbone and every rank **above** the anchor
#' (`Family`, `Order`, `Class`, ...) is overwritten with the backbone lineage.
#'
#' This makes taxonomies obtained from **different reference databases**
#' comparable: two databases often agree on a species or genus name but then
#' diverge on the higher ranks (different `Class`, `Family`, ...). Re-deriving
#' those higher ranks from one common backbone removes that spurious divergence.
#'
#' Several databases can be harmonised at once through **suffix-based tracks**.
#' For example, if UNITE assignments live in the plain columns (`Kingdom`,
#' `Genus`, ...) and Eukaryome assignments live in suffixed columns
#' (`Kingdom_Euk`, `Genus_Euk`, ...), pass `suffixes = c("", "_Euk")` and both
#' tracks are harmonised independently against the same backbone.
#'
#' @details
#' **Anchor.** With `anchor = "last_assigned"` (default), each taxon is anchored
#' at its deepest assigned rank (the right-most non-missing rank of the track,
#' e.g. `Genus` when `Species` is `NA`). With an explicit rank
#' (`anchor = "Genus"`), that rank is used for every taxon of every track (the
#' track suffix is appended, so the `_Euk` track is anchored on `Genus_Euk`).
#'
#' **Match gating.** A backbone match is applied only when it is trustworthy:
#' `matchType` must be in `match_types` and `confidence` at least
#' `min_confidence`. Taxa that fail the gate keep their original ranks and are
#' reported as unresolved, so a poor match never corrupts existing data.
#' Ambiguous or homonymous names (e.g. `"Boletus"`) that GBIF resolves only to a
#' higher rank (`matchType = "HIGHERRANK"`) are therefore left unchanged by
#' default. `"Boletus"` is a real example: although GBIF does hold an accepted
#' fungal genus `Boletus`, the bare string is too ambiguous to place (a large
#' tangle of fungal synonyms across several families, plus minor cross-kingdom
#' string collisions), so the matcher backs off to `HIGHERRANK` (kingdom) rather
#' than commit -- and a `rank` hint biases the scorer but does not override that
#' back-off. Anchoring deeper resolves such cases -- the binomial
#' `"Boletus edulis"` matches `EXACT` -- which is why `anchor = "last_assigned"`
#' (which queries the `Genus species` binomial at the species level) is the
#' default. To recover such names at the anchor rank itself, set
#' `resolve_ambiguous = TRUE`: the failed names are re-queried with the verbose
#' [rgbif::name_backbone()] alternatives and the accepted candidate at the
#' requested rank is kept (so `"Boletus"` resolves to the accepted genus in
#' Boletaceae). You can also relax `match_types` (e.g. `c("EXACT", "HIGHERRANK")`,
#' still safe because ranks the backbone returns as `NA` are never written),
#' lower `min_confidence`, or harmonise the names first with [gna_verifier_pq()]
#' and anchor on the resulting canonical column.
#'
#' **Homonym disambiguation.** A bare name can resolve in several kingdoms
#' (a genus name shared by fungi and animals). Pass `kingdom` (e.g. `"Fungi"`)
#' to constrain the backbone query and avoid pulling the wrong lineage.
#'
#' **Only higher ranks change.** The anchor rank and every rank *below* it are
#' left untouched; only ranks strictly above the anchor are re-derived. Ranks in
#' `ranks` that the backbone does not provide are skipped.
#'
#' @param physeq (phyloseq, required) A phyloseq object with a `tax_table`.
#' @param anchor (character of length 1, default `"last_assigned"`) Either
#'   `"last_assigned"` (use each taxon's deepest assigned rank) or the name of a
#'   single rank present in `ranks` (e.g. `"Genus"`).
#' @param suffixes (character, default `""`) One entry per taxonomy track. Each
#'   suffix is appended verbatim to the `ranks` names to locate that track's
#'   columns (e.g. `"_Euk"` targets `Genus_Euk`, `Family_Euk`, ...). The default
#'   `""` harmonises the plain rank columns only.
#' @param ranks (character, default the seven Linnaean ranks) The canonical rank
#'   order, from the highest (left) to the lowest (right). Determines which
#'   columns count as "above" the anchor.
#' @param kingdom (character of length 1, default `NULL`) Optional kingdom used
#'   to disambiguate every backbone query (e.g. `"Fungi"`). Ignored when a local
#'   `backbone` is supplied.
#' @param backbone (data.frame, default `NULL`) Optional **local** backbone to
#'   use instead of querying GBIF online. Must contain a `name` column (matched
#'   against the anchor values) and one column per rank named as in `ranks`
#'   (case-insensitive). When supplied, every matching row is treated as a
#'   trusted `EXACT` match (the online match gating is bypassed).
#' @param min_confidence (numeric of length 1, default `80`) Minimum GBIF
#'   `confidence` required to apply an online backbone match. Ignored for a
#'   local `backbone`.
#' @param match_types (character, default `"EXACT"`) GBIF `matchType` values
#'   accepted for an online match. Ignored for a local `backbone`.
#' @param resolve_ambiguous (logical, default `FALSE`) If `TRUE`, anchor names
#'   that the fast [rgbif::name_backbone_checklist()] fails to place (e.g. a bare
#'   ambiguous genus that backs off to `HIGHERRANK`) are retried per name with
#'   [rgbif::name_backbone()] and `verbose = TRUE`; among the returned
#'   alternatives, the accepted candidate at the requested rank (and `kingdom`
#'   when given) with the highest `confidence` is used. This recovers names such
#'   as `"Boletus"` (chosen as the accepted genus in Boletaceae) but issues one
#'   extra API call per unresolved name, so it is slower. Ignored for a local
#'   `backbone`.
#' @param keep_original (logical, default `FALSE`) If `TRUE`, the pre-existing
#'   value of every overwritten column is preserved in a companion column named
#'   `<column><original_suffix>`.
#' @param original_suffix (character of length 1, default `"_orig"`) Suffix of
#'   the companion columns created when `keep_original = TRUE`.
#' @param verbose (logical, default `TRUE`) If `TRUE`, print a summary of the
#'   changes with \pkg{cli}.
#'
#' @return The phyloseq object with harmonised higher-rank columns in its
#'   `tax_table` (original taxa order and names preserved).
#' @export
#' @author Adrien Taudière
#'
#' @seealso [tax_crosscheck_pq()], [gna_verifier_pq()],
#'   [rgbif::name_backbone_checklist()]
#'
#' @examples
#' library(phyloseq)
#'
#' # A tiny object with a WRONG Family/Order/Class for Amanita (divergent DB),
#' # and a second (Eukaryome) track carrying the same problem.
#' tax <- matrix(
#'   c(
#'     # Kingdom  Class          Order          Family         Genus
#'     "Fungi", "WrongClass", "WrongOrder", "WrongFamily", "Amanita",
#'     "Fungi", NA, NA, NA, "Boletus"
#'   ),
#'   nrow = 2,
#'   byrow = TRUE,
#'   dimnames = list(
#'     c("ASV1", "ASV2"),
#'     c("Kingdom", "Class", "Order", "Family", "Genus")
#'   )
#' )
#' otu <- matrix(
#'   c(5, 1, 0, 3),
#'   nrow = 2,
#'   dimnames = list(c("ASV1", "ASV2"), c("s1", "s2"))
#' )
#' pq <- phyloseq(
#'   otu_table(otu, taxa_are_rows = TRUE),
#'   tax_table(tax)
#' )
#'
#' # A local backbone (no network needed) keyed by genus name.
#' backbone <- data.frame(
#'   name = c("Amanita", "Boletus"),
#'   Kingdom = "Fungi",
#'   Class = "Agaricomycetes",
#'   Order = c("Agaricales", "Boletales"),
#'   Family = c("Amanitaceae", "Boletaceae"),
#'   Genus = c("Amanita", "Boletus")
#' )
#'
#' # Anchor on Genus: Kingdom/Class/Order/Family are re-derived from the backbone.
#' harmonised <- tax_harmonize_backbone_pq(
#'   pq,
#'   anchor = "Genus",
#'   backbone = backbone
#' )
#' as.data.frame(tax_table(harmonised))
#'
#' \dontrun{
#'  data_fungi_mini2 <- assign_dada2(data_fungi_mini,
#'   ref_fasta = system.file("extdata", "mini_UNITE_fungi.fasta.gz",
#'      package = "MiscMetabar"
#'    ), suffix = "_dada2",
#'    from_sintax = TRUE
#'  )
#'  add_new_taxonomy_pq(data_fungi_mini, ref_fasta, method = "dada2")
#' # Online GBIF backbone, restricted to fungi, on two database tracks at once:
#' data_fungi_mini <-
#' tax_harmonize_backbone_pq(
#'   data_fungi_mini,
#'   anchor = "last_assigned",
#'   kingdom = "Fungi"
#' )
#' }
tax_harmonize_backbone_pq <- function(
  physeq,
  anchor = "last_assigned",
  suffixes = "",
  ranks = c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species"
  ),
  kingdom = NULL,
  backbone = NULL,
  min_confidence = 80,
  match_types = c("EXACT"),
  resolve_ambiguous = FALSE,
  keep_original = FALSE,
  original_suffix = "_orig",
  verbose = TRUE
) {
  verify_pq(physeq)

  if (length(anchor) != 1 || !is.character(anchor)) {
    cli::cli_abort("{.arg anchor} must be a single character string.")
  }
  if (anchor != "last_assigned" && !anchor %in% ranks) {
    cli::cli_abort(c(
      "{.arg anchor} must be {.val last_assigned} or one of {.val {ranks}}.",
      "x" = "Got {.val {anchor}}."
    ))
  }
  if (!is.character(suffixes) || length(suffixes) == 0) {
    cli::cli_abort("{.arg suffixes} must be a non-empty character vector.")
  }

  tax <- as.data.frame(
    unclass(physeq@tax_table),
    stringsAsFactors = FALSE
  )
  original_tax <- tax

  # Build the per-taxon, per-track plan: which anchor rank/value each taxon
  # uses and which higher-rank columns must be re-derived.
  plan <- harmonize_build_plan(tax, anchor, suffixes, ranks)
  if (nrow(plan$queries) == 0) {
    cli::cli_warn(
      "No taxon has a usable anchor value; the object is returned unchanged."
    )
    return(physeq)
  }

  # Resolve every unique (name, rank) query against the backbone.
  lineage <- harmonize_resolve(
    plan$queries,
    kingdom = kingdom,
    backbone = backbone,
    ranks = ranks,
    min_confidence = min_confidence,
    match_types = match_types,
    resolve_ambiguous = resolve_ambiguous,
    verbose = verbose
  )

  # Apply the resolved lineage, overwriting only ranks above each anchor.
  n_changed <- 0L
  n_unresolved <- 0L
  for (i in seq_len(nrow(plan$fills))) {
    row <- plan$fills[i, ]
    key <- paste(row$anchor_value, tolower(row$anchor_rank), sep = "\r")
    hit <- lineage[[key]]
    if (is.null(hit)) {
      n_unresolved <- n_unresolved + 1L
      next
    }
    target_col <- row$target_col
    new_value <- hit[[tolower(row$rank)]]
    if (is.null(new_value) || is.na(new_value)) {
      next
    }
    if (keep_original) {
      orig_col <- paste0(target_col, original_suffix)
      if (is.null(tax[[orig_col]])) {
        tax[[orig_col]] <- original_tax[[target_col]]
      }
    }
    if (is.null(tax[[target_col]])) {
      tax[[target_col]] <- NA_character_
    }
    old_value <- tax[[target_col]][row$taxon_idx]
    if (is.na(old_value) || old_value != new_value) {
      n_changed <- n_changed + 1L
    }
    tax[[target_col]][row$taxon_idx] <- new_value
  }

  new_physeq <- physeq
  new_tax <- as.matrix(tax)
  rownames(new_tax) <- rownames(original_tax)
  new_physeq@tax_table <- phyloseq::tax_table(new_tax)

  if (verbose) {
    cli::cli_alert_success(
      "Harmonised {.val {n_changed}} rank cell{?s} across {.val {length(suffixes)}} track{?s}."
    )
    if (n_unresolved > 0) {
      cli::cli_alert_warning(
        "{.val {n_unresolved}} anchor lookup{?s} unresolved (kept original ranks)."
      )
    }
  }

  verify_pq(new_physeq)
  new_physeq
}

#' Build the per-taxon harmonisation plan
#'
#' Returns the set of unique backbone queries and a long table of (taxon, track,
#' target column, rank-to-fill) fill instructions.
#'
#' @param tax A tax_table coerced to a data.frame.
#' @param anchor `"last_assigned"` or a single rank name.
#' @param suffixes Character vector of track suffixes.
#' @param ranks Canonical rank order (highest first).
#' @return A list with `queries` (data.frame: anchor_value, anchor_rank) and
#'   `fills` (data.frame: taxon_idx, anchor_value, anchor_rank, rank,
#'   target_col).
#' @keywords internal
#' @noRd
harmonize_build_plan <- function(tax, anchor, suffixes, ranks) {
  fills <- list()
  for (suffix in suffixes) {
    track_cols <- if (suffix == "") {
      ranks
    } else {
      paste0(ranks, suffix)
    }
    names(track_cols) <- ranks
    present <- track_cols[track_cols %in% colnames(tax)]
    if (length(present) == 0) {
      cli::cli_warn(
        "Track suffix {.val {suffix}} matches no column; skipped."
      )
      next
    }
    present_ranks <- names(present)

    if (anchor != "last_assigned" && !anchor %in% present_ranks) {
      cli::cli_warn(
        "Anchor rank {.val {anchor}} absent from track {.val {suffix}}; skipped."
      )
      next
    }

    for (taxon_idx in seq_len(nrow(tax))) {
      anchor_rank <- harmonize_anchor_rank(
        tax,
        taxon_idx,
        anchor,
        present,
        present_ranks
      )
      if (is.na(anchor_rank)) {
        next
      }
      anchor_col <- present[[anchor_rank]]
      anchor_value <- tax[[anchor_col]][taxon_idx]
      if (is.na(anchor_value) || !nzchar(trimws(anchor_value))) {
        next
      }
      # A bare species epithet is meaningless to a backbone: query the
      # binomial (Genus + Species) unless the cell already carries the genus.
      if (anchor_rank == "Species" && "Genus" %in% present_ranks) {
        genus_value <- tax[[present[["Genus"]]]][taxon_idx]
        if (
          !is.na(genus_value) &&
            nzchar(trimws(genus_value)) &&
            !startsWith(anchor_value, genus_value)
        ) {
          anchor_value <- paste(genus_value, anchor_value)
        }
      }
      # Ranks strictly above the anchor, restricted to this track's columns.
      above <- ranks[seq_len(match(anchor_rank, ranks) - 1L)]
      above <- above[above %in% present_ranks]
      for (rk in above) {
        fills[[length(fills) + 1L]] <- data.frame(
          taxon_idx = taxon_idx,
          anchor_value = anchor_value,
          anchor_rank = anchor_rank,
          rank = rk,
          target_col = present[[rk]],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  fills_df <- if (length(fills) > 0) {
    do.call(rbind, fills)
  } else {
    data.frame(
      taxon_idx = integer(0),
      anchor_value = character(0),
      anchor_rank = character(0),
      rank = character(0),
      target_col = character(0),
      stringsAsFactors = FALSE
    )
  }

  queries <- unique(fills_df[, c("anchor_value", "anchor_rank"), drop = FALSE])

  list(queries = queries, fills = fills_df)
}

#' Determine a taxon's anchor rank within one track
#'
#' @param tax tax_table data.frame.
#' @param taxon_idx Row index.
#' @param anchor `"last_assigned"` or a rank name.
#' @param present Named vector: rank -> existing column name for the track.
#' @param present_ranks Ranks (names of `present`) present in the track.
#' @return The anchor rank name, or `NA` when none is usable.
#' @keywords internal
#' @noRd
harmonize_anchor_rank <- function(
  tax,
  taxon_idx,
  anchor,
  present,
  present_ranks
) {
  if (anchor != "last_assigned") {
    return(anchor)
  }
  # Deepest assigned = right-most present rank with a non-empty value.
  for (rk in rev(present_ranks)) {
    value <- tax[[present[[rk]]]][taxon_idx]
    if (!is.na(value) && nzchar(trimws(value))) {
      return(rk)
    }
  }
  NA_character_
}

#' Resolve anchor queries against a local or online backbone
#'
#' @param queries data.frame with `anchor_value` and `anchor_rank`.
#' @param kingdom Optional kingdom filter (online only).
#' @param backbone Optional local backbone data.frame.
#' @param ranks Canonical rank order.
#' @param min_confidence,match_types Online match gating.
#' @param verbose Emit progress messages.
#' @return A named list keyed by `"<name>\r<rank_lower>"`, each element a named
#'   list of lower-case rank -> value for the accepted match.
#' @keywords internal
#' @noRd
harmonize_resolve <- function(
  queries,
  kingdom,
  backbone,
  ranks,
  min_confidence,
  match_types,
  resolve_ambiguous,
  verbose
) {
  lower_ranks <- tolower(ranks)

  if (!is.null(backbone)) {
    if (!"name" %in% colnames(backbone)) {
      cli::cli_abort(
        "A local {.arg backbone} must have a {.field name} column."
      )
    }
    bb <- backbone
    colnames(bb) <- tolower(colnames(bb))
    out <- list()
    for (i in seq_len(nrow(queries))) {
      nm <- queries$anchor_value[i]
      rk <- tolower(queries$anchor_rank[i])
      match_row <- bb[bb$name == nm, , drop = FALSE]
      if (nrow(match_row) == 0) {
        next
      }
      key <- paste(nm, rk, sep = "\r")
      out[[key]] <- as.list(match_row[1, intersect(lower_ranks, colnames(bb))])
    }
    return(out)
  }

  # Online GBIF backbone.
  if (verbose) {
    cli::cli_alert_info(
      "Querying GBIF backbone for {.val {nrow(queries)}} unique anchor name{?s}..."
    )
  }
  query_df <- data.frame(
    name = queries$anchor_value,
    rank = tolower(queries$anchor_rank),
    stringsAsFactors = FALSE
  )
  if (!is.null(kingdom)) {
    query_df$kingdom <- kingdom
  }
  res <- rgbif::name_backbone_checklist(query_df)

  out <- list()
  for (i in seq_len(nrow(res))) {
    row <- res[i, ]
    nm <- query_df$name[i]
    rk <- query_df$rank[i]
    key <- paste(nm, rk, sep = "\r")

    passes <- !is.na(row$matchType) &&
      row$matchType %in% match_types &&
      !is.na(row$confidence) &&
      row$confidence >= min_confidence
    if (passes) {
      out[[key]] <- as.list(row[intersect(lower_ranks, colnames(res))])
      next
    }

    # Fast best-match failed (e.g. an ambiguous bare name backed off to
    # HIGHERRANK). Optionally recover via the verbose alternatives, keeping
    # only the accepted candidate at the requested rank.
    if (resolve_ambiguous) {
      hit <- harmonize_recover_one(
        name = nm,
        rank = rk,
        kingdom = kingdom,
        min_confidence = min_confidence,
        match_types = match_types
      )
      if (!is.null(hit)) {
        out[[key]] <- as.list(hit[intersect(lower_ranks, colnames(hit))])
      }
    }
  }
  out
}

#' Recover an ambiguous anchor via the verbose GBIF alternatives
#'
#' Queries [rgbif::name_backbone()] with `verbose = TRUE` and returns the single
#' best alternative at the requested rank (see [harmonize_pick_candidate()]), or
#' `NULL` when none qualifies.
#'
#' @param name Anchor name to resolve.
#' @param rank Requested rank, lower-case (e.g. `"genus"`).
#' @param kingdom Optional kingdom filter.
#' @param min_confidence,match_types Match gating passed to the candidate picker.
#' @return A one-row data.frame of the chosen candidate, or `NULL`.
#' @keywords internal
#' @noRd
harmonize_recover_one <- function(
  name,
  rank,
  kingdom,
  min_confidence,
  match_types
) {
  alts <- tryCatch(
    as.data.frame(rgbif::name_backbone(
      name = name,
      rank = rank,
      kingdom = kingdom,
      verbose = TRUE
    )),
    error = function(e) NULL
  )
  harmonize_pick_candidate(
    alts,
    want_rank = rank,
    kingdom = kingdom,
    min_confidence = min_confidence,
    match_types = match_types
  )
}

#' Pick the best rank-matched candidate from GBIF verbose alternatives
#'
#' Pure selection logic (no network) shared by [harmonize_recover_one()]. Keeps
#' only alternatives at `want_rank` (and `kingdom` when given) that pass the
#' `match_types` / `min_confidence` gate, then prefers `ACCEPTED` status and the
#' highest `confidence`. This avoids selecting a same-rank *synonym* that would
#' point to the wrong lineage.
#'
#' @param alts A data.frame of alternatives (as returned by
#'   [rgbif::name_backbone()] with `verbose = TRUE`), or `NULL`.
#' @param want_rank Requested rank, lower-case; matched case-insensitively
#'   against the alternatives' `rank` column.
#' @param kingdom Optional kingdom filter.
#' @param min_confidence,match_types Match gating.
#' @return A one-row data.frame of the chosen candidate, or `NULL`.
#' @keywords internal
#' @noRd
harmonize_pick_candidate <- function(
  alts,
  want_rank,
  kingdom,
  min_confidence,
  match_types
) {
  if (is.null(alts) || nrow(alts) == 0 || !"rank" %in% colnames(alts)) {
    return(NULL)
  }
  keep <- !is.na(alts$rank) & toupper(alts$rank) == toupper(want_rank)
  if (!is.null(kingdom) && "kingdom" %in% colnames(alts)) {
    keep <- keep & !is.na(alts$kingdom) & alts$kingdom == kingdom
  }
  if ("matchType" %in% colnames(alts)) {
    keep <- keep & !is.na(alts$matchType) & alts$matchType %in% match_types
  }
  if ("confidence" %in% colnames(alts)) {
    keep <- keep & !is.na(alts$confidence) & alts$confidence >= min_confidence
  }
  cand <- alts[keep, , drop = FALSE]
  if (nrow(cand) == 0) {
    return(NULL)
  }
  accepted <- if ("status" %in% colnames(cand)) {
    as.integer(!is.na(cand$status) & cand$status == "ACCEPTED")
  } else {
    rep(0L, nrow(cand))
  }
  conf <- if ("confidence" %in% colnames(cand)) {
    cand$confidence
  } else {
    rep(0, nrow(cand))
  }
  cand <- cand[order(-accepted, -conf), , drop = FALSE]
  cand[1, , drop = FALSE]
}
