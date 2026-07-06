#' Add metaTraits phenotypic traits to a phyloseq object
#'
#' @description
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Augments the `tax_table` slot of a phyloseq object with harmonised microbial
#' phenotypic traits from the metaTraits resource (Robbani et al. 2026,
#' <https://metatraits.embl.de>). metaTraits integrates culture-derived trait
#' information (BacDive, BV-BRC, JGI IMG, GOLD) with genome-based predictions
#' over GTDB r220, covering more than 140 traits (cell morphology, motility,
#' sporulation, oxygen/temperature/pH/salinity preferences, metabolism, ...).
#'
#' Traits are matched on GTDB taxon names. Because the resource is native to
#' GTDB, environmental lineages known only from GTDB placeholder names (e.g.
#' `JAJZYD01`) are matched just as well as classically named taxa, which makes
#' metaTraits particularly suited to MAG-based archaeal/bacterial datasets.
#'
#' Matching is done species-first: when a taxon's `Species` name is present in
#' the species-level summary its traits are used, and any trait missing at the
#' species level falls back to the genus-level summary. The (large) summary
#' tables are downloaded once and cached in a per-user directory
#' (`tools::R_user_dir("taxinfo", "cache")`).
#'
#' @param physeq (required) A phyloseq object.
#' @param genus_rank (Character, default `"Genus"`) Column of `tax_table`
#'   holding the GTDB genus name.
#' @param species_rank (Character, default `"Species"`) Column of `tax_table`
#'   holding the GTDB species name.
#' @param level (Character vector, default `c("species", "genus")`) Taxonomic
#'   levels to query, in order of preference. Use `"genus"` alone to skip the
#'   large (~140 MB) species download.
#' @param traits (Character vector or `NULL`) Trait names (metaTraits
#'   `trait_name`) to keep. `NULL` (default) keeps every trait found for the
#'   matched taxa.
#' @param groups (Character vector or `NULL`) If supplied, keep only traits
#'   whose metaTraits `group_1` category is in this vector (e.g.
#'   `"Metabolism"`, `"Environmental preferences"`). Applied on top of
#'   `traits`.
#' @param min_consensus_percentage (Numeric, default `0`) Drop trait values
#'   whose `consensus_percentage` is below this threshold (set them to `NA`).
#' @param taxonomy (Character, default `"gtdb"`) Taxonomy of the summary files.
#'   Only `"gtdb"` supports name-based joins and is currently implemented.
#' @param no_predictions (Logical, default `FALSE`) If `TRUE`, use the
#'   culture-based-only summary files (without genome-based predictions).
#' @param col_prefix (Character, default `"mt_"`) Prefix applied to all trait
#'   columns added to the `tax_table`.
#' @param add_to_phyloseq (Logical, default `TRUE`) If `TRUE`, return an updated
#'   phyloseq object. If `FALSE`, return a tibble of the augmented `tax_table`.
#' @param cache_dir (Character) Directory used to cache the downloaded summary
#'   files. Defaults to `tools::R_user_dir("taxinfo", "cache")`.
#' @param refresh (Logical, default `FALSE`) If `TRUE`, re-download the summary
#'   files even if a cached copy exists.
#' @param verbose (Logical, default `TRUE`) If `TRUE`, print progress messages.
#'
#' @returns Either an updated phyloseq object (when `add_to_phyloseq = TRUE`)
#'   or a tibble of the augmented `tax_table`. A `mt_trait_level` column records
#'   whether each taxon's traits came from the `"species"` or `"genus"` summary
#'   (`NA` when unmatched).
#'
#' @references
#' Robbani, S. M. et al. (2026). metaTraits: a large-scale integration of
#' microbial phenotypic trait information. *Nucleic Acids Research*, 54(D1),
#' D835. \doi{10.1093/nar/gkaf1080}
#'
#' @author Adrien Taudiere
#' @export
#'
#' @seealso [add_faprotax_pq()], [fungal_traits_guilds()], [tax_info_pq()]
#'
#' @examples
#' \dontrun{
#' # GlobalPatterns is a bacterial/archaeal 16S dataset shipped with phyloseq.
#' # metaTraits joins on GTDB names, so coverage is highest when the tax_table
#' # already carries GTDB taxonomy (see e.g. tax_harmonize_backbone_pq()).
#' data(GlobalPatterns, package = "phyloseq")
#'
#' # Genus + species matching (downloads ~40 MB + ~140 MB once, then cached)
#' res <- add_metatraits_pq(GlobalPatterns)
#' table(res@tax_table[, "mt_trait_level"], useNA = "always")
#'
#' # Genus only, restricted to metabolism traits (no species download)
#' res_g <- add_metatraits_pq(GlobalPatterns, level = "genus", groups = "Metabolism")
#' table(res_g@tax_table[, "mt_oxygen preference"], useNA = "always")
#'
#' # Return a tibble instead of a phyloseq object
#' tib <- add_metatraits_pq(GlobalPatterns, level = "genus", add_to_phyloseq = FALSE)
#' }
add_metatraits_pq <- function(
  physeq,
  genus_rank = "Genus",
  species_rank = "Species",
  level = c("species", "genus"),
  traits = NULL,
  groups = NULL,
  min_consensus_percentage = 0,
  taxonomy = "gtdb",
  no_predictions = FALSE,
  col_prefix = "mt_",
  add_to_phyloseq = TRUE,
  cache_dir = tools::R_user_dir("taxinfo", "cache"),
  refresh = FALSE,
  verbose = TRUE
) {
  if (is.null(physeq) || !methods::is(physeq, "phyloseq")) {
    cli::cli_abort("{.arg physeq} must be a {.cls phyloseq} object.")
  }
  taxonomy <- match.arg(taxonomy, c("gtdb"))
  level <- match.arg(level, c("species", "genus"), several.ok = TRUE)

  if (!genus_rank %in% colnames(physeq@tax_table)) {
    cli::cli_abort(
      "Genus column {.val {genus_rank}} not found in the {.field tax_table}."
    )
  }
  has_species <- species_rank %in% colnames(physeq@tax_table)
  if ("species" %in% level && !has_species) {
    cli::cli_warn(
      "Species column {.val {species_rank}} not found; using genus level only."
    )
    level <- setdiff(level, "species")
  }
  if (length(level) == 0) {
    cli::cli_abort("No usable taxonomic {.arg level} left to query.")
  }

  tax_df <- as.data.frame(unclass(physeq@tax_table), stringsAsFactors = FALSE)

  # Step 1: build clean GTDB join keys ---------------------------------------
  genus_key <- mt_clean_genus(tax_df[[genus_rank]])
  species_key <- if (has_species) {
    mt_clean_species(genus_key, tax_df[[species_rank]])
  } else {
    rep(NA_character_, nrow(tax_df))
  }

  # Step 2: fetch + pivot the requested summaries ----------------------------
  genus_wide <- NULL
  species_wide <- NULL

  if ("genus" %in% level) {
    keys <- unique(stats::na.omit(genus_key))
    genus_wide <- mt_load_wide(
      level = "genus",
      keys = keys,
      taxonomy = taxonomy,
      no_predictions = no_predictions,
      traits = traits,
      groups = groups,
      min_consensus_percentage = min_consensus_percentage,
      cache_dir = cache_dir,
      refresh = refresh,
      verbose = verbose
    )
  }
  if ("species" %in% level) {
    keys <- unique(stats::na.omit(species_key))
    species_wide <- mt_load_wide(
      level = "species",
      keys = keys,
      taxonomy = taxonomy,
      no_predictions = no_predictions,
      traits = traits,
      groups = groups,
      min_consensus_percentage = min_consensus_percentage,
      cache_dir = cache_dir,
      refresh = refresh,
      verbose = verbose
    )
  }

  # Step 3: assemble per-taxon trait rows (species-first, genus-fallback) ----
  n_taxa <- nrow(tax_df)
  trait_cols <- union(
    setdiff(colnames(species_wide), "taxon_name"),
    setdiff(colnames(genus_wide), "taxon_name")
  )

  result <- matrix(
    NA_character_,
    nrow = n_taxa,
    ncol = length(trait_cols),
    dimnames = list(NULL, trait_cols)
  )
  trait_level <- rep(NA_character_, n_taxa)

  # Genus values first (fallback layer).
  if (!is.null(genus_wide)) {
    gm <- match(genus_key, genus_wide$taxon_name)
    for (col in setdiff(colnames(genus_wide), "taxon_name")) {
      result[, col] <- genus_wide[[col]][gm]
    }
    trait_level[!is.na(gm)] <- "genus"
  }
  # Species values override where present (preferred layer).
  if (!is.null(species_wide)) {
    sm <- match(species_key, species_wide$taxon_name)
    for (col in setdiff(colnames(species_wide), "taxon_name")) {
      sval <- species_wide[[col]][sm]
      result[!is.na(sval), col] <- sval[!is.na(sval)]
    }
    trait_level[!is.na(sm)] <- "species"
  }

  new_cols <- as.data.frame(result, stringsAsFactors = FALSE)
  if (ncol(new_cols) > 0) {
    colnames(new_cols) <- paste0(col_prefix, colnames(new_cols))
  }
  new_cols[[paste0(col_prefix, "trait_level")]] <- trait_level

  # Suffix any column that already exists in the tax_table so that re-running
  # the function (or an earlier annotation) never produces duplicate names.
  colnames(new_cols) <- disambiguate_new_cols(
    colnames(physeq@tax_table),
    colnames(new_cols),
    verbose = verbose
  )
  rownames(new_cols) <- taxa_names(physeq)

  if (verbose) {
    cli::cli_alert_success(
      "Added {.val {ncol(new_cols)}} metaTraits column{?s} for {.val {sum(!is.na(trait_level))}}/{.val {n_taxa}} taxa ({.val {sum(trait_level == 'species', na.rm = TRUE)}} at species level)."
    )
  }

  new_tax_tab <- cbind(as.data.frame(physeq@tax_table), new_cols)
  new_physeq <- physeq
  new_physeq@tax_table <- tax_table(as.matrix(new_tax_tab))
  rownames(new_physeq@tax_table) <- taxa_names(physeq)

  if (add_to_phyloseq) {
    return(new_physeq)
  } else {
    return(tibble::as_tibble(as.data.frame(new_physeq@tax_table)))
  }
}

# Internal helpers -------------------------------------------------------------

#' Make new tax_table column names unique with respect to existing ones
#'
#' Shared by [add_faprotax_pq()] and [add_metatraits_pq()]. Any `new_names` that
#' already appears in `existing` is suffixed (`_1`, `_2`, ...) so that
#' re-running an annotation never yields duplicate `tax_table` column names.
#' @noRd
disambiguate_new_cols <- function(existing, new_names, verbose = TRUE) {
  clashing <- intersect(new_names, existing)
  if (length(clashing) == 0) {
    return(new_names)
  }
  uniq <- make.unique(c(existing, new_names), sep = "_")
  out <- uniq[-seq_along(existing)]
  if (verbose) {
    cli::cli_alert_info(
      "{length(clashing)} column{?s} already present in the {.field tax_table} {?was/were} suffixed to avoid duplicates: {.val {clashing}}."
    )
  }
  out
}

#' Clean a GTDB genus vector into metaTraits `taxon_name` keys
#' @noRd
mt_clean_genus <- function(x) {
  x <- trimws(gsub("^\\s*g__\\s*", "", as.character(x)))
  x[is.na(x) | x %in% c("", "?")] <- NA_character_
  x
}

#' Rebuild GTDB species `taxon_name` keys ("<genus> <epithet>")
#'
#' `simplify_taxo()` and GTDB exports use inconsistent separators between the
#' genus and the epithet ("BOG-1369 sp003", "BOG-1369sp003",
#' "Methanocella_A_arvoryzae"). Using the already-clean genus as an anchor,
#' strip it (plus one optional separator) from the species string and glue it
#' back with a single space, matching the metaTraits GTDB format.
#' @noRd
mt_clean_species <- function(genus_key, species) {
  s <- trimws(gsub("^\\s*s__\\s*", "", as.character(species)))
  out <- rep(NA_character_, length(s))
  valid <- !is.na(s) & !s %in% c("", "?")
  for (i in which(valid)) {
    g <- genus_key[i]
    if (!is.na(g) && nzchar(g)) {
      remainder <- sub(
        paste0("^", mt_regex_escape(g), "[ _]?"),
        "",
        s[i]
      )
      out[i] <- paste0(g, " ", remainder)
    } else {
      out[i] <- gsub("_", " ", s[i])
    }
  }
  out
}

#' Escape regex metacharacters in a literal string
#' @noRd
mt_regex_escape <- function(x) {
  gsub("([.\\\\+*?\\[\\^\\]$(){}=!<>|:#/-])", "\\\\\\1", x, perl = TRUE)
}

#' Download (once) and return the path to a metaTraits summary file
#' @noRd
mt_download <- function(
  level,
  taxonomy,
  no_predictions,
  cache_dir,
  refresh,
  verbose
) {
  suffix <- if (no_predictions) "_no_predictions" else "_all"
  fname <- sprintf("%s_%s_summary%s.tsv.gz", taxonomy, level, suffix)
  url <- paste0("https://www.bork.embl.de/~robbani/metatraits/", fname)
  dest <- file.path(cache_dir, fname)

  if (refresh || !file.exists(dest)) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    if (verbose) {
      approx <- if (level == "species") "~140 MB" else "~40 MB"
      cli::cli_inform(c(
        "i" = "Downloading metaTraits {.val {level}} summary ({approx}) to {.path {cache_dir}}.",
        " " = "This happens once; subsequent calls reuse the cached file."
      ))
    }
    utils::download.file(url, dest, mode = "wb", quiet = !verbose)
  }
  dest
}

#' Stream-filter a metaTraits summary file to a set of taxa and pivot to wide
#'
#' Reads the (large) gzipped long-format file in chunks, keeping only the rows
#' whose `taxon_name` is in `keys`, then pivots the retained rows to one row
#' per taxon with one column per `trait_name` (cell = `consensus_value`).
#' @noRd
mt_load_wide <- function(
  level,
  keys,
  taxonomy,
  no_predictions,
  traits,
  groups,
  min_consensus_percentage,
  cache_dir,
  refresh,
  verbose
) {
  if (length(keys) == 0) {
    return(NULL)
  }
  path <- mt_download(
    level = level,
    taxonomy = taxonomy,
    no_predictions = no_predictions,
    cache_dir = cache_dir,
    refresh = refresh,
    verbose = verbose
  )

  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)

  header <- strsplit(readLines(con, n = 1L), "\t", fixed = TRUE)[[1]]
  idx <- function(name) match(name, header)
  i_taxon <- idx("taxon_name")
  i_trait <- idx("trait_name")
  i_value <- idx("consensus_value")
  i_pct <- idx("consensus_percentage")
  i_group1 <- idx("group_1")

  keyset <- keys
  kept <- list()
  chunk <- 200000L
  repeat {
    lines <- readLines(con, n = chunk)
    if (length(lines) == 0) {
      break
    }
    parts <- strsplit(lines, "\t", fixed = TRUE)
    taxa <- vapply(
      parts,
      function(p) if (length(p) >= i_taxon) p[i_taxon] else NA_character_,
      character(1)
    )
    keep <- taxa %in% keyset
    if (any(keep)) {
      kept[[length(kept) + 1]] <- parts[keep]
    }
  }

  if (length(kept) == 0) {
    return(NULL)
  }
  kept <- unlist(kept, recursive = FALSE)

  get_field <- function(p, i) {
    vapply(p, function(x) if (length(x) >= i) x[i] else NA_character_, character(1))
  }
  long <- data.frame(
    taxon_name = get_field(kept, i_taxon),
    trait_name = get_field(kept, i_trait),
    consensus_value = get_field(kept, i_value),
    consensus_percentage = suppressWarnings(as.numeric(get_field(kept, i_pct))),
    group_1 = get_field(kept, i_group1),
    stringsAsFactors = FALSE
  )

  # Optional filters ---------------------------------------------------------
  if (!is.null(traits)) {
    long <- long[long$trait_name %in% traits, , drop = FALSE]
  }
  if (!is.null(groups)) {
    long <- long[long$group_1 %in% groups, , drop = FALSE]
  }
  if (min_consensus_percentage > 0) {
    drop <- !is.na(long$consensus_percentage) &
      long$consensus_percentage < min_consensus_percentage
    long$consensus_value[drop] <- NA_character_
  }
  long <- long[!is.na(long$consensus_value), , drop = FALSE]
  if (nrow(long) == 0) {
    return(NULL)
  }

  # Pivot to wide: one row per taxon, one column per trait -------------------
  long <- long[!duplicated(long[, c("taxon_name", "trait_name")]), , drop = FALSE]
  wide <- tidyr::pivot_wider(
    long[, c("taxon_name", "trait_name", "consensus_value")],
    names_from = "trait_name",
    values_from = "consensus_value"
  )
  wide <- as.data.frame(wide, stringsAsFactors = FALSE)

  if (verbose) {
    cli::cli_alert_success(
      "metaTraits {.val {level}}: matched {.val {nrow(wide)}} taxon name{?s}, {.val {ncol(wide) - 1}} trait{?s}."
    )
  }
  wide
}
