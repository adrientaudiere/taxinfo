#' Add fungal spore volume and morphology to a phyloseq object
#'
#' @description
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Annotates the taxa of a phyloseq object with fungal spore volume and
#' morphology (length, width, projected area, and length/width ratio) from the
#' spore-trait database compiled by Aguilar-Trigueros et al. (2023) and
#' redistributed by the `q2-fungal-traits` QIIME 2 plugin.
#'
#' Matching follows the same hierarchical strategy as the upstream plugin: for
#' every requested spore type, a taxon is first matched at the **species** level
#' (exact match on the `genus species` binomial), then, if no species match is
#' found, at the **genus** level, and finally at the **family** level. Genus- and
#' family-level values are the geometric mean (`10^mean(log10(x))`) of every
#' database entry for that rank and spore type, exactly as in the plugin. The
#' matched rank is reported in a `*_matching_level` column. Only taxa whose
#' kingdom/domain is *Fungi* are matched (when a kingdom column is available),
#' preventing cross-kingdom genus-name collisions.
#'
#' @details
#' The bundled `Spore_data_12Nov21.tsv` is redistributed verbatim from the
#' `q2-fungal-traits` repository
#' (<https://github.com/bokulich-lab/q2-fungal-traits>), which distributes it
#' under the Modified BSD (3-clause) License. The underlying data were compiled
#' by Aguilar-Trigueros et al. (2023). Taxon names are normalised for matching
#' the same way the plugin does: square brackets are removed, `-` and `_` are
#' turned into spaces, whitespace is squished, and names are lower-cased.
#'
#' The database covers roughly 25,000 species, 4,200 genera and 600 families,
#' so genus- and family-level fallback matches make the annotation useful even
#' for metabarcoding data resolved only to genus.
#'
#' @param physeq (required) A phyloseq object.
#' @param spore_file (Character) Path to the spore-trait TSV file. Defaults to
#'   the version bundled with the package
#'   (`system.file("extdata", "Spore_data_12Nov21.tsv", package = "taxinfo")`).
#' @param genus_rank,species_rank,family_rank (Character) Names of the
#'   `tax_table` columns holding the genus, species epithet and family. The
#'   `species` binomial used for species-level matching is built from
#'   `genus_rank` and `species_rank` (the genus is prepended automatically when
#'   the species column holds only the epithet).
#' @param kingdom_rank (Character or `NULL`, default `NULL`) Name of the
#'   `tax_table` column holding the kingdom/domain. When `NULL`, the columns
#'   `"Kingdom"` and `"Domain"` are looked up automatically. When found, only
#'   taxa whose value normalises to `"fungi"` are matched. When no such column
#'   exists, the kingdom guard is silently skipped.
#' @param spore_types (Character vector) Spore types to annotate. Defaults to all
#'   four types present in the database: `"Mitospores"`, `"Meiospores"`,
#'   `"Multinucleate sexual spores"` and `"Multinucleate asexual spores"`.
#' @param metrics (Character vector) Spore-trait columns to import for each spore
#'   type. Defaults to `"SporeVolume"`, `"spore_length"`, `"spore_width"`,
#'   `"SporeArea"` and `"Q_ratio"`.
#' @param col_prefix (Character, default `"spore_"`) Prefix applied to all
#'   columns added to the `tax_table`. New columns are named
#'   `<col_prefix><spore_type>_<metric>` (e.g. `spore_mitospores_volume`) plus
#'   one `<col_prefix><spore_type>_matching_level` column per spore type.
#' @param add_to_phyloseq (Logical, default `TRUE`) If `TRUE`, return an updated
#'   phyloseq object. If `FALSE`, return a tibble with one row per taxon.
#' @param verbose (Logical, default `TRUE`) If `TRUE`, print progress messages.
#'
#' @returns Either an updated phyloseq object (when `add_to_phyloseq = TRUE`)
#'   or a tibble of the matched values (when `add_to_phyloseq = FALSE`).
#'
#' @references
#' Aguilar-Trigueros, C. A., Krah, F.-S., Cornwell, W. K., Zanne, A. E.,
#' Abrego, N., Anderson, I. C., ... & Bassler, C. (2023). Symbiotic status
#' alters fungal eco-evolutionary offspring trajectories. *Ecology Letters*,
#' 26(9), 1523-1534. \doi{10.1111/ele.14271}
#'
#' @author Adrien Taudiere
#' @export
#'
#' @seealso [tax_spores_size_pq()], [fungal_traits_guilds()],
#'   [tax_faprotax_pq()], [tax_info_pq()]
#'
#' @examples
#' res <- tax_spores_volume_pq(data_fungi_mini, verbose = FALSE)
#' table(res@tax_table[, "spore_meiospores_matching_level"], useNA = "always")
#'
#' # Return a tibble instead of a phyloseq object
#' tib <- tax_spores_volume_pq(
#'   data_fungi_mini,
#'   add_to_phyloseq = FALSE,
#'   verbose = FALSE
#' )
#'
#' tidypq::pq_to_tidy(res) |>
#'   filter(abundance > 10) |>
#'   ggplot2::ggplot(ggplot2::aes(x=Height, color=Height, size = as.numeric(spore_meiospores_volume), y = log10(abundance))) +
#'   ggplot2::geom_jitter()
#'
#'
tax_spores_volume_pq <- function(
  physeq,
  spore_file = system.file(
    "extdata",
    "Spore_data_12Nov21.tsv",
    package = "taxinfo"
  ),
  genus_rank = "Genus",
  species_rank = "Species",
  family_rank = "Family",
  kingdom_rank = NULL,
  spore_types = c(
    "Mitospores",
    "Meiospores",
    "Multinucleate sexual spores",
    "Multinucleate asexual spores"
  ),
  metrics = c(
    "SporeVolume",
    "spore_length",
    "spore_width",
    "SporeArea",
    "Q_ratio"
  ),
  col_prefix = "spore_",
  add_to_phyloseq = TRUE,
  verbose = TRUE
) {
  verify_pq(physeq)

  if (!file.exists(spore_file)) {
    cli::cli_abort("Spore database file not found at {.path {spore_file}}.")
  }

  spore_data <- utils::read.delim(
    spore_file,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Validate requested spore types and metrics -------------------------------
  missing_types <- setdiff(spore_types, unique(spore_data$SporeType))
  if (length(missing_types) > 0) {
    cli::cli_abort(
      "Spore type{?s} not found in the database: {.val {missing_types}}."
    )
  }
  missing_metrics <- setdiff(metrics, colnames(spore_data))
  if (length(missing_metrics) > 0) {
    cli::cli_abort(
      "Metric column{?s} not found in the database: {.val {missing_metrics}}."
    )
  }

  tax_ranks <- colnames(physeq@tax_table)
  for (rk in c(genus_rank, species_rank, family_rank)) {
    if (!rk %in% tax_ranks) {
      cli::cli_abort(
        "Column {.val {rk}} not found in the {.field tax_table}."
      )
    }
  }

  tax_df <- as.data.frame(
    unclass(physeq@tax_table),
    stringsAsFactors = FALSE
  )

  # Build the normalised database keys ---------------------------------------
  spore_data$species_spd_key <- normalize_taxon_key(spore_data$names_to_use)
  spore_data$genus_spd_key <- normalize_taxon_key(spore_data$genus)
  spore_data$family_spd_key <- normalize_taxon_key(spore_data$family)

  # Build the taxonomy keys --------------------------------------------------
  genus_key <- normalize_taxon_key(tax_df[[genus_rank]])
  family_key <- normalize_taxon_key(tax_df[[family_rank]])
  species_raw <- normalize_taxon_key(tax_df[[species_rank]])
  first_word <- sub(" .*", "", species_raw)
  species_key <- ifelse(
    is.na(species_raw) | is.na(genus_key) | first_word == genus_key,
    species_raw,
    paste(genus_key, species_raw)
  )

  # Kingdom guard ------------------------------------------------------------
  if (is.null(kingdom_rank)) {
    kingdom_rank <- intersect(c("Kingdom", "Domain"), tax_ranks)[1]
  }
  if (!is.null(kingdom_rank) && !is.na(kingdom_rank)) {
    fungal <- normalize_taxon_key(tax_df[[kingdom_rank]]) == "fungi"
    fungal[is.na(fungal)] <- FALSE
  } else {
    fungal <- rep(TRUE, nrow(tax_df))
  }

  metric_slug <- c(
    SporeVolume = "volume",
    spore_length = "length",
    spore_width = "width",
    SporeArea = "area",
    Q_ratio = "q_ratio"
  )

  new_cols <- list()

  for (spore_type in spore_types) {
    db <- spore_data[spore_data$SporeType == spore_type, , drop = FALSE]

    sp_agg <- agg_spore_level(db, "species_spd_key", metrics)
    gen_agg <- agg_spore_level(db, "genus_spd_key", metrics)
    fam_agg <- agg_spore_level(db, "family_spd_key", metrics)

    i_sp <- match(species_key, sp_agg$key)
    i_gen <- match(genus_key, gen_agg$key)
    i_fam <- match(family_key, fam_agg$key)

    level <- rep(NA_character_, nrow(tax_df))
    level[fungal & !is.na(i_sp)] <- "species"
    level[fungal & is.na(level) & !is.na(i_gen)] <- "genus"
    level[fungal & is.na(level) & !is.na(i_fam)] <- "family"

    type_slug <- tolower(gsub(" ", "_", spore_type))

    for (m in metrics) {
      val <- rep(NA_real_, nrow(tax_df))
      is_sp <- !is.na(level) & level == "species"
      is_gen <- !is.na(level) & level == "genus"
      is_fam <- !is.na(level) & level == "family"
      val[is_sp] <- sp_agg[[m]][i_sp[is_sp]]
      val[is_gen] <- gen_agg[[m]][i_gen[is_gen]]
      val[is_fam] <- fam_agg[[m]][i_fam[is_fam]]
      new_cols[[paste0(col_prefix, type_slug, "_", metric_slug[[m]])]] <- val
    }
    new_cols[[paste0(col_prefix, type_slug, "_matching_level")]] <- level

    if (verbose) {
      cli::cli_alert_info(
        "{.val {spore_type}}: {sum(!is.na(level))}/{nrow(tax_df)} taxa matched
        ({sum(level == 'species', na.rm = TRUE)} species,
        {sum(level == 'genus', na.rm = TRUE)} genus,
        {sum(level == 'family', na.rm = TRUE)} family)."
      )
    }
  }

  new_df <- as.data.frame(
    new_cols,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Collision check ----------------------------------------------------------
  clash <- intersect(colnames(new_df), tax_ranks)
  if (length(clash) > 0) {
    cli::cli_abort(c(
      "New column{?s} {.val {clash}} already exist in the {.field tax_table}.",
      "i" = "Pass a different {.arg col_prefix} to disambiguate."
    ))
  }

  if (add_to_phyloseq) {
    combined <- cbind(tax_df, new_df)
    new_physeq <- physeq
    new_physeq@tax_table <- phyloseq::tax_table(as.matrix(combined))
    rownames(new_physeq@tax_table) <- phyloseq::taxa_names(physeq)
    return(new_physeq)
  } else {
    out <- tibble::as_tibble(cbind(
      taxa_name = phyloseq::taxa_names(physeq),
      new_df
    ))
    return(out)
  }
}

# Internal helpers -------------------------------------------------------------

#' Normalise a taxon name for cross-dataset matching
#'
#' Reproduces the `normalize_taxon_key` logic of the `q2-fungal-traits` plugin:
#' remove square brackets, turn `-`/`_` into spaces, squish whitespace,
#' lower-case, and map empty strings to `NA`.
#' @param x A character vector.
#' @returns A normalised character vector.
#' @noRd
normalize_taxon_key <- function(x) {
  x <- as.character(x)
  x <- gsub("[][]", "", x)
  x <- gsub("[-_]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x <- tolower(x)
  x[x == ""] <- NA_character_
  x
}

#' Geometric-mean aggregation of spore metrics for one taxonomic rank
#'
#' Groups `db` by `key_col` and returns, for each requested metric, the
#' geometric mean (`10^mean(log10(x))`) over the strictly positive values.
#' @param db A data frame of spore records (one spore type).
#' @param key_col Name of the normalised key column to group by.
#' @param metrics Character vector of metric columns to aggregate.
#' @returns A data frame with a `key` column and one column per metric.
#' @noRd
agg_spore_level <- function(db, key_col, metrics) {
  db <- db[!is.na(db[[key_col]]), , drop = FALSE]
  keys <- unique(db[[key_col]])
  out <- data.frame(key = keys, stringsAsFactors = FALSE)
  split_idx <- split(seq_len(nrow(db)), db[[key_col]])[keys]
  for (m in metrics) {
    vals <- suppressWarnings(as.numeric(db[[m]]))
    out[[m]] <- vapply(
      split_idx,
      function(idx) {
        v <- vals[idx]
        v <- v[!is.na(v) & v > 0]
        if (length(v) == 0) {
          NA_real_
        } else {
          10^mean(log10(v))
        }
      },
      numeric(1)
    )
  }
  out
}
