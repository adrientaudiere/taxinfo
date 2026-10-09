#' Standardized effect size of sample altitude against GBIF altitudes
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' For each taxon, compare the altitude of the samples where it is present
#' with the altitudes of its GBIF occurrences, using a standardized effect
#' size (SES, a z-score):
#'
#' `SES = (sample_alt_mean - gbif_alt_mean) / gbif_alt_sd`
#'
#' where `sample_alt_mean` is the mean altitude of the samples in which the
#' taxon is present (presence/absence, not weighted by abundance) and
#' `gbif_alt_mean` / `gbif_alt_sd` are the mean and standard deviation of the
#' altitudes of its GBIF occurrences. A positive SES means that the taxon was
#' found higher than usual according to GBIF, a negative SES lower than usual.
#'
#' @param physeq (phyloseq, required) A phyloseq object with a `sam_data`
#'   slot.
#' @param sample_alt_col (character, required) Name of the numeric column of
#'   the `sam_data` slot giving the altitude of each sample (in meters).
#' @param occ_alt (data.frame, required) GBIF altitudes of the taxa, either:
#'   - a long table with one row per occurrence (as returned by
#'     [tax_gbif_alt()] with `return_raw = TRUE`), with columns `taxa_col`
#'     and `elevation_col`;
#'   - or a per-taxon summary (as returned by [tax_gbif_alt()] with
#'     `add_to_phyloseq = FALSE`), with columns `taxa_col`, `altitude_mean`,
#'     `altitude_sd` and `altitude_n_records`.
#' @param taxonomic_rank (character, default `"currentCanonicalSimple"`) The
#'   column(s) of the `tax_table` giving the taxonomic names, matched against
#'   the `taxa_col` column of `occ_alt`. Can be a vector of two columns (e.g.
#'   `c("Genus", "Species")`).
#' @param taxa_col (character, default `"canonicalName"`) Name of the column of
#'   `occ_alt` with the taxonomic names.
#' @param elevation_col (character, default `"elevation"`) Name of the column
#'   of `occ_alt` with the occurrence altitudes (long format only).
#'
#' @returns A tibble with one row per taxonomic name of `physeq`, sorted by
#'   decreasing SES: `taxa_name`, `n_samples` (number of samples with an
#'   altitude where the taxon is present), `sample_alt_mean`, `gbif_alt_mean`,
#'   `gbif_alt_sd`, `gbif_n_records` and `ses` (`NA` when the taxon has no
#'   GBIF altitude, is absent from all samples, or when `gbif_alt_sd` is `NA`
#'   or zero).
#' @export
#' @author Adrien Taudiere
#' @seealso [plot_tax_alt_pq()], [tax_gbif_alt()]
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini,
#'   data_sources = 11
#' )
#' # Requires GBIF credentials (see ?tax_gbif_alt)
#' occ_alt <- tax_gbif_alt(data_fungi_mini_cleanNames, return_raw = TRUE)
#'
#' # data_fungi_mini has no altitude: add a fictive one
#' sample_data(data_fungi_mini_cleanNames)$altitude <-
#'   runif(nsamples(data_fungi_mini_cleanNames), 0, 1500)
#' tax_alt_ses_pq(data_fungi_mini_cleanNames, "altitude", occ_alt)
#' }
tax_alt_ses_pq <- function(
  physeq,
  sample_alt_col,
  occ_alt,
  taxonomic_rank = "currentCanonicalSimple",
  taxa_col = "canonicalName",
  elevation_col = "elevation"
) {
  sample_alt <- sample_altitudes(physeq, sample_alt_col)
  if (!is.data.frame(occ_alt) || !taxa_col %in% colnames(occ_alt)) {
    cli::cli_abort(
      "{.arg occ_alt} must be a data.frame with a {.val {taxa_col}} column."
    )
  }

  if (elevation_col %in% colnames(occ_alt)) {
    gbif_stats <- tibble(
      taxa_name = as.character(occ_alt[[taxa_col]]),
      elevation = as.numeric(occ_alt[[elevation_col]])
    ) |>
      filter(!is.na(.data$elevation)) |>
      summarise(
        gbif_alt_mean = mean(.data$elevation),
        gbif_alt_sd = stats::sd(.data$elevation),
        gbif_n_records = n(),
        .by = "taxa_name"
      )
  } else if (
    all(
      c("altitude_mean", "altitude_sd", "altitude_n_records") %in%
        colnames(occ_alt)
    )
  ) {
    gbif_stats <- tibble(
      taxa_name = as.character(occ_alt[[taxa_col]]),
      gbif_alt_mean = as.numeric(occ_alt$altitude_mean),
      gbif_alt_sd = as.numeric(occ_alt$altitude_sd),
      gbif_n_records = as.numeric(occ_alt$altitude_n_records)
    ) |>
      filter(!duplicated(.data$taxa_name))
  } else {
    cli::cli_abort(c(
      "{.arg occ_alt} must have either an {.val {elevation_col}} column (one row per occurrence)",
      "or the {.val altitude_mean}, {.val altitude_sd} and {.val altitude_n_records} columns of {.fn tax_gbif_alt}."
    ))
  }

  presence <- taxa_presence_by_name(physeq, taxonomic_rank)
  presence <- presence[, names(sample_alt)[!is.na(sample_alt)], drop = FALSE]
  alt_ok <- sample_alt[colnames(presence)]
  n_samples <- unname(rowSums(presence))

  tibble(
    taxa_name = rownames(presence),
    n_samples = n_samples,
    sample_alt_mean = as.vector(presence %*% alt_ok) / n_samples
  ) |>
    mutate(
      sample_alt_mean = if_else(
        .data$n_samples > 0,
        .data$sample_alt_mean,
        NA_real_
      )
    ) |>
    left_join(gbif_stats, by = "taxa_name") |>
    mutate(
      ses = if_else(
        !is.na(.data$gbif_alt_sd) & .data$gbif_alt_sd > 0,
        (.data$sample_alt_mean - .data$gbif_alt_mean) / .data$gbif_alt_sd,
        NA_real_
      )
    ) |>
    arrange(desc(.data$ses))
}

#' Violin plot of GBIF altitudes with the altitude of the samples
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' For each taxon, draw the distribution of the altitudes of its GBIF
#' occurrences as a violin, and the altitude of each sample in which the taxon
#' is present as a horizontal line. The standardized effect size (SES)
#' computed by [tax_alt_ses_pq()] is written to the right of the highest
#' altitude of each taxon, and taxa are
#' ordered by SES.
#'
#' @inheritParams tax_alt_ses_pq
#' @param occ_alt (data.frame, required) GBIF altitudes of the taxa, with one
#'   row per occurrence (as returned by [tax_gbif_alt()] with
#'   `return_raw = TRUE`), with columns `taxa_col` and `elevation_col`.
#' @param min_records (integer, default `10`) Minimum number of GBIF
#'   altitudes for a taxon to be plotted.
#' @param order_by_ses (logical, default `TRUE`) If TRUE, taxa are ordered by
#'   increasing SES, else alphabetically.
#' @param show_ses (logical, default `TRUE`) If TRUE, the SES is written to the right
#'   of the highest altitude of each taxon (GBIF occurrences and samples).
#' @param sample_color (character, default `"#C0392B"`) Color of the lines
#'   depicting the altitude of the samples.
#' @param point_jitter (logical, default `TRUE`) If TRUE each altitude point is plot
#'   on the graph. 
#'
#' @returns A ggplot2 object.
#' @export
#' @author Adrien Taudiere
#' @seealso [tax_alt_ses_pq()], [tax_gbif_alt()]
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini,
#'   data_sources = 11
#' )
#' # Requires GBIF credentials (see ?tax_gbif_alt)
#' occ_alt <- tax_gbif_alt(data_fungi_mini_cleanNames, return_raw = TRUE)
#'
#' # data_fungi_mini has no altitude: add a fictive one value
#' sample_data(data_fungi_mini_cleanNames)$altitude <- 150
#' 
#' 
#' plot_tax_alt_pq(data_fungi_mini_cleanNames, "altitude", occ_alt)
#' }
plot_tax_alt_pq <- function(
  physeq,
  sample_alt_col,
  occ_alt,
  taxonomic_rank = "currentCanonicalSimple",
  taxa_col = "canonicalName",
  elevation_col = "elevation",
  min_records = 10,
  order_by_ses = TRUE,
  show_ses = TRUE,
  sample_color = "#C0392B",
  points_jitter = TRUE
) {
  if (!is.data.frame(occ_alt) || !elevation_col %in% colnames(occ_alt)) {
    cli::cli_abort(c(
      "{.arg occ_alt} must have one row per occurrence with an {.val {elevation_col}} column.",
      "i" = "Use {.code tax_gbif_alt(return_raw = TRUE)}."
    ))
  }
  ses <- tax_alt_ses_pq(
    physeq,
    sample_alt_col = sample_alt_col,
    occ_alt = occ_alt,
    taxonomic_rank = taxonomic_rank,
    taxa_col = taxa_col,
    elevation_col = elevation_col
  ) |>
    filter(.data$n_samples > 0, .data$gbif_n_records >= min_records)

  if (nrow(ses) == 0) {
    cli::cli_abort(c(
      "No taxon is both present in a sample with an altitude and has at least {.val {min_records}} GBIF altitude{?s}.",
      "i" = "Check that {.arg taxonomic_rank} matches the names of {.arg occ_alt}, or lower {.arg min_records}."
    ))
  }

  taxa_levels <- if (order_by_ses) {
    ses$taxa_name[order(ses$ses, na.last = FALSE)]
  } else {
    sort(ses$taxa_name)
  }

  occ_df <- tibble(
    taxa_name = as.character(occ_alt[[taxa_col]]),
    elevation = as.numeric(occ_alt[[elevation_col]])
  ) |>
    filter(.data$taxa_name %in% taxa_levels, !is.na(.data$elevation)) |>
    mutate(taxa_name = factor(.data$taxa_name, levels = taxa_levels))

  sample_alt <- sample_altitudes(physeq, sample_alt_col)
  presence <- taxa_presence_by_name(physeq, taxonomic_rank)[
    taxa_levels,
    ,
    drop = FALSE
  ]
  samples_df <- which(presence, arr.ind = TRUE) |>
    as.data.frame() |>
    transmute(
      taxa_name = factor(taxa_levels[.data$row], levels = taxa_levels),
      sample_alt = unname(sample_alt[colnames(presence)[.data$col]])
    ) |>
    filter(!is.na(.data$sample_alt))

  p <- ggplot(occ_df, aes(x = .data$taxa_name, y = .data$elevation))
  
  if(points_jitter) {
    p <- p+ geom_jitter(color="grey50", size =0.4, alpha= 0.5)
  }

  p <- p +
    geom_violin(fill = scales::alpha("blue", 0.2), color = "grey40", scale = "width") +
    geom_errorbar(
      data = samples_df,
      aes(
        x = .data$taxa_name,
        ymin = .data$sample_alt,
        ymax = .data$sample_alt
      ),
      inherit.aes = FALSE,
      width = 0.7,
      color = sample_color,
      linewidth = 0.8
    ) +
    scale_x_italic_species() +
    labs(
      x = NULL,
      y = "Altitude (m)",
      title = "GBIF altitudes of taxa vs. altitude of samples",
      subtitle = "Violins: altitude of GBIF occurrences; lines: altitude of samples where the taxon is present"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (show_ses) {
    max_alt <- bind_rows(
      select(occ_df, "taxa_name", alt = "elevation"),
      select(samples_df, "taxa_name", alt = "sample_alt")
    ) |>
      summarise(max_alt = max(.data$alt), .by = "taxa_name")

    # After coord_flip(), the last level is the top line of the plot
    ses_df <- ses |>
      mutate(taxa_name = factor(.data$taxa_name, levels = taxa_levels)) |>
      left_join(max_alt, by = "taxa_name") |>
      mutate(
        label = paste0(
          if_else(
            as.integer(.data$taxa_name) == length(taxa_levels),
            "SES = ",
            ""
          ),
          formatC(.data$ses, format = "f", digits = 2)
        )
      )
    p <- p +
      geom_text(
        data = ses_df,
        aes(x = .data$taxa_name, y = .data$max_alt, label = .data$label),
        inherit.aes = FALSE,
        hjust = 0,
        nudge_y = 0.01 * diff(range(max_alt$max_alt, occ_df$elevation)),
        size = 3
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
  }
  p <- p + coord_flip(clip = "off")
  p
}

#' Numeric altitude of each sample, named by sample
#'
#' @param physeq A phyloseq object.
#' @param sample_alt_col Name of the altitude column of the `sam_data` slot.
#' @returns A named numeric vector.
#' @noRd
sample_altitudes <- function(physeq, sample_alt_col) {
  if (!inherits(physeq, "phyloseq") || is.null(physeq@sam_data)) {
    cli::cli_abort(
      "{.arg physeq} must be a phyloseq object with a sam_data slot."
    )
  }
  if (!sample_alt_col %in% phyloseq::sample_variables(physeq)) {
    cli::cli_abort(
      "{.arg sample_alt_col} = {.val {sample_alt_col}} is not a column of the sam_data slot."
    )
  }
  alt <- suppressWarnings(as.numeric(as.character(
    physeq@sam_data[[sample_alt_col]]
  )))
  if (all(is.na(alt))) {
    cli::cli_abort(
      "The {.val {sample_alt_col}} column of the sam_data slot has no numeric value."
    )
  }
  stats::setNames(alt, phyloseq::sample_names(physeq))
}

#' Presence of each taxonomic name in each sample
#'
#' ASVs sharing the same taxonomic name are merged.
#'
#' @param physeq A phyloseq object.
#' @param taxonomic_rank The `tax_table` column(s) giving the names.
#' @returns A logical matrix, taxonomic names in rows and samples in columns.
#' @noRd
taxa_presence_by_name <- function(physeq, taxonomic_rank) {
  taxa_name <- taxnames_from_rank(
    physeq@tax_table,
    taxonomic_rank,
    clean = TRUE
  )
  otu <- as(phyloseq::otu_table(physeq), "matrix")
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu <- t(otu)
  }
  keep <- !is.na(taxa_name) & !taxa_name %in% c("", "NA")
  rowsum((otu[keep, , drop = FALSE] > 0) * 1, group = taxa_name[keep]) > 0
}
