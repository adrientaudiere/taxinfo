#' Add FungalTraits and FUNGuild information to a phyloseq object
#'
#' @description
#' A convenience wrapper that adds guild and trait information from both the
#' FungalTraits database and the FUNGuild database to the `tax_table` slot of
#' a phyloseq object. Optionally creates consensus columns that summarise
#' agreement between the two databases.
#'
#' If `currentCanonicalSimple` is not already present in the `tax_table`,
#' [gna_verifier_pq()] is called internally to clean and verify the taxonomic
#' names before querying the databases.
#'
#' @param physeq A phyloseq object.
#' @param fungal_traits_file (Character) Path to the FungalTraits CSV file.
#'   Defaults to the simplified version bundled with the package.
#' @param ft_taxonomic_rank (Character, default `"genusEpithet"`) Column in
#'   `tax_table` used to match against the FungalTraits genus column.
#' @param ft_csv_rank (Character, default `"GENUS"`) Column in the FungalTraits
#'   CSV file that contains genus names.
#' @param ft_sep (Character, default `";"`) Field separator of the FungalTraits
#'   CSV file. See [utils::read.csv()].
#' @param ft_col_prefix (Character, default `"ft_"`) Prefix applied to all
#'   columns imported from FungalTraits.
#' @param fg_tax_levels (Character vector) Names of the tax_table columns that
#'   represent the 7 standard taxonomic ranks fed to FUNGuild.
#' @param fg_col_prefix (Character, default `"fg_"`) Prefix applied to all
#'   columns imported from FUNGuild.
#' @param db_url (Character) URL of the FUNGuild database.
#'   See [MiscMetabar::get_funguild_db()].
#' @param add_consensus (Logical, default `TRUE`) If `TRUE`, add consensus
#'   columns comparing trophic modes assigned by the two databases.
#' @param consensus_col_prefix (Character, default `"cons_"`) Prefix applied to
#'   consensus columns.
#' @param add_to_phyloseq (Logical, default `TRUE`) If `TRUE`, return an
#'   updated phyloseq object. If `FALSE`, return a tibble of the tax_table.
#' @param gna_data_sources Integer or character vector passed to
#'   [gna_verifier_pq()] when taxonomic names need to be verified.
#'   See [taxize::gna_verifier()].
#' @param verbose (Logical, default `TRUE`) If `TRUE`, print progress messages.
#'
#' @returns Either an updated phyloseq object (when `add_to_phyloseq = TRUE`)
#'   or a tibble of the augmented tax_table.
#' @author Adrien Taudiere
#' @export
#'
#' @seealso [tax_info_pq()], [gna_verifier_pq()],
#'   [MiscMetabar::add_funguild_info()], [MiscMetabar::funguild_assign()]
#'
#' @examples
#' # physeq object with already-verified names
#' res_guild <- data_fungi |>
#'  gna_verifier_pq(data_sources = 210) |>
#'    fungal_traits_guilds()
#'
#' table(res_guild@tax_table[, "cons_trophicMode"], useNA = "always")
#' table(res_guild@tax_table[, "cons_trophicMode_agreement"], useNA = "always")
#'
#' # physeq object WITHOUT verified names: gna_verifier_pq is called internally
#' res_guild_2 <- fungal_traits_guilds(data_fungi, gna_data_sources = 210)
#' table(res_guild_2@tax_table[, "ft_primary_lifestyle"])
#' table(res_guild_2@tax_table[, "fg_trophicMode"])
#' table(res_guild_2@tax_table[, "cons_trophicMode"])
#'
#' # Return a tibble instead of a phyloseq
#' tib <- fungal_traits_guilds(data_fungi_cleanNames, add_to_phyloseq = FALSE)
#' 
#' \donttest{
#' res_guild_2 |> psmelt() |>
#'  filter(Abundance > 0) |>
#'  ggplot(aes(x = Height, y = Abundance, fill = cons_trophicMode)) +
#'  geom_col() +
#'  theme_bw() +
#'  labs(x = "Height", y = "Molecular abundance", fill = "Consensus trophic mode") +
#'  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#' 
#' tax_bar_pq(res_guild_2,"Height", "cons_trophicMode", add_ribbon=TRUE) 
#' }

fungal_traits_guilds <- function(
  physeq,
  fungal_traits_file = system.file(
    "extdata",
    "fungal_traits.csv",
    package = "taxinfo"
  ),
  ft_taxonomic_rank = "genusEpithet",
  ft_csv_rank = "GENUS",
  ft_sep = "\t",
  ft_col_prefix = "ft_",
  fg_tax_levels = c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species"
  ),
  fg_col_prefix = "fg_",
  ft_csv_cols_select = c("GENUS", "COMMENT.on.genus", "primary_lifestyle", "Secondary_lifestyle", "Comment_on_lifestyle_template", "Endophytic_interaction_capability_template", "Plant_pathogenic_capacity_template", "Decay_substrate_template", "Decay_type_template", "Aquatic_habitat_template", "Animal_biotrophic_capacity_template", "Specific_hosts", "Growth_form_template", "Fruitbody_type_template", "Hymenium_type_template", "Ectomycorrhiza_exploration_type_template", "Ectomycorrhiza_lineage_template", "primary_photobiont", "secondary_photobiont"),
  db_url = "http://www.stbates.org/funguild_db_2.php",
  add_consensus = TRUE,
  consensus_col_prefix = "cons_",
  add_to_phyloseq = TRUE,
  gna_data_sources = c(1, 12),
  verbose = TRUE
) {
  check_package("httr")

  # Step 0: Verify names if currentCanonicalSimple is not yet present ----------
  if (!("currentCanonicalSimple" %in% colnames(physeq@tax_table))) {
    if (verbose) {
      cli::cli_alert_info(
        "Column {.val currentCanonicalSimple} not found in tax_table.",
        " Running {.fn gna_verifier_pq} first."
      )
    }
    physeq <- gna_verifier_pq(
      physeq,
      data_sources = gna_data_sources,
      verbose = verbose
    )
  }

  if (!(ft_taxonomic_rank %in% colnames(physeq@tax_table))) {
    cli::cli_abort(
      c(
        "Column {.val {ft_taxonomic_rank}} not found in tax_table.",
        "i" = "This column is expected after {.fn gna_verifier_pq}.",
        "i" = "Check {.arg ft_taxonomic_rank} or run {.fn gna_verifier_pq} manually."
      )
    )
  }

  new_physeq <- physeq

  # Step 1: Add FungalTraits data ----------------------------------------------
  new_physeq <- tax_info_pq(
    new_physeq,
    taxonomic_rank = ft_taxonomic_rank,
    file_name = fungal_traits_file,
    csv_taxonomic_rank = ft_csv_rank,
    col_prefix = ft_col_prefix,
    csv_cols_select = ft_csv_cols_select,
    sep = ft_sep,
    verbose = verbose,
    discard_genus_alone = FALSE
  )

  # Step 2: Add FUNGuild data --------------------------------------------------
  fg_result <- NULL
  valid_tax_levels <- fg_tax_levels[
    fg_tax_levels %in% colnames(new_physeq@tax_table)
  ]

  if (length(valid_tax_levels) == 0) {
    cli::cli_warn(
      c(
        "None of the {.arg fg_tax_levels} columns found in tax_table.",
        "i" = "Skipping FUNGuild annotation."
      )
    )
  } else {
    if (length(valid_tax_levels) < length(fg_tax_levels)) {
      cli::cli_warn(
        "Some {.arg fg_tax_levels} not found in tax_table: {.val {setdiff(fg_tax_levels, valid_tax_levels)}}."
      )
    }

    if (httr::http_error(db_url)) {
      cli::cli_warn(
        "Cannot access FUNGuild database at {.url {db_url}}. Skipping FUNGuild."
      )
    } else {
      fg_db <- MiscMetabar::get_funguild_db(db_url = db_url)

      if (!is.null(fg_db)) {
        taxa_keys <- taxa_names(new_physeq)
        tax_df <- data.frame(
          .taxa_key = taxa_keys,
          Taxonomy = apply(
            new_physeq@tax_table[, valid_tax_levels],
            1,
            paste,
            collapse = ";"
          ),
          stringsAsFactors = FALSE
        )

        fg_assigned <- MiscMetabar::funguild_assign(
          tax_df,
          db_funguild = fg_db,
          tax_col = "Taxonomy"
        )

        fg_new_cols <- setdiff(
          colnames(fg_assigned),
          c("Taxonomy", ".taxa_key")
        )
        # De-duplicate: funguild_assign may expand rows when multiple DB entries
        # share the same searchkey. Keep only the first match per taxon.
        fg_assigned_dedup <- fg_assigned[
          !duplicated(fg_assigned$.taxa_key),
          ,
          drop = FALSE
        ]
        fg_result <- fg_assigned_dedup[
          match(taxa_keys, fg_assigned_dedup$.taxa_key),
          fg_new_cols,
          drop = FALSE
        ]
        rownames(fg_result) <- taxa_keys

        if (!is.null(fg_col_prefix) && nchar(fg_col_prefix) > 0) {
          colnames(fg_result) <- paste0(fg_col_prefix, colnames(fg_result))
        }

        new_tax_tab <- cbind(
          as.data.frame(new_physeq@tax_table),
          fg_result
        )
        new_physeq@tax_table <- tax_table(as.matrix(new_tax_tab))
        rownames(new_physeq@tax_table) <- taxa_names(physeq)

        if (verbose) {
          cli::cli_alert_success(
            "Added {.val {length(fg_new_cols)}} FUNGuild column{?s} to tax_table."
          )
        }
      }
    }
  }

  # Step 3: Add consensus columns ----------------------------------------------
  if (add_consensus && !is.null(fg_result)) {
    ft_lifestyle_col <- paste0(ft_col_prefix, "primary_lifestyle")
    fg_trophic_col <- paste0(fg_col_prefix, "trophicMode")
    fg_guild_col <- paste0(fg_col_prefix, "guild")

    tax_df_cons <- as.data.frame(new_physeq@tax_table)
    cons_cols_added <- character(0)

    has_ft_lifestyle <- ft_lifestyle_col %in% colnames(tax_df_cons)
    has_fg_trophic <- fg_trophic_col %in% colnames(tax_df_cons)
    has_fg_guild <- fg_guild_col %in% colnames(tax_df_cons)

    if (has_ft_lifestyle && has_fg_trophic) {
      ft_norm <- ft_to_trophic_mode(tax_df_cons[[ft_lifestyle_col]]) # nolint: object_usage_linter.
      fg_norm <- tax_df_cons[[fg_trophic_col]]
      fg_norm[fg_norm == ""] <- NA_character_

      cons_trophic_col <- paste0(consensus_col_prefix, "trophicMode")
      tax_df_cons[[cons_trophic_col]] <- dplyr::case_when(
        is.na(ft_norm) & is.na(fg_norm) ~ NA_character_,
        is.na(ft_norm) ~ fg_norm,
        is.na(fg_norm) ~ ft_norm,
        grepl(ft_norm, fg_norm, fixed = TRUE) ~ ft_norm,
        ft_norm == fg_norm ~ ft_norm,
        .default = "Conflicting"
      )
      cons_cols_added <- c(cons_cols_added, cons_trophic_col)
    }

    if (has_ft_lifestyle && has_fg_guild && has_fg_trophic) {
      ft_norm2 <- ft_to_trophic_mode(tax_df_cons[[ft_lifestyle_col]]) # nolint: object_usage_linter.
      fg_norm2 <- tax_df_cons[[fg_trophic_col]]
      fg_norm2[fg_norm2 == ""] <- NA_character_

      agree_col <- paste0(consensus_col_prefix, "trophicMode_agreement")
      tax_df_cons[[agree_col]] <- dplyr::case_when(
        is.na(ft_norm2) | is.na(fg_norm2) ~ "Only one source",
        grepl(ft_norm2, fg_norm2, fixed = TRUE) |
          ft_norm2 == fg_norm2 ~ "Agree",
        .default = "Disagree"
      )
      cons_cols_added <- c(cons_cols_added, agree_col)
    }

    new_physeq@tax_table <- tax_table(as.matrix(tax_df_cons))
    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    if (verbose && length(cons_cols_added) > 0) {
      cli::cli_alert_success(
        "Added {.val {length(cons_cols_added)}} consensus column{?s}:
        {.val {cons_cols_added}}."
      )
    }
  }

  if (add_to_phyloseq) {
    return(new_physeq)
  } else {
    return(tibble::as_tibble(as.data.frame(new_physeq@tax_table)))
  }
}

# Internal helpers -------------------------------------------------------------

#' Normalise FungalTraits primary_lifestyle to trophic mode categories matching
#' FUNGuild's trophicMode vocabulary (Saprotroph / Pathotroph / Symbiotroph).
#' @noRd
ft_to_trophic_mode <- function(x) {
  dplyr::case_when(
    x %in%
      c(
        "dung_saprotroph",
        "litter_saprotroph",
        "pollen_saprotroph",
        "soil_saprotroph",
        "unspecified_saprotroph",
        "wood_saprotroph"
      ) ~ "Saprotroph",
    x %in%
      c(
        "algal_parasite",
        "animal_parasite",
        "lichen_parasite",
        "mycoparasite",
        "plant_pathogen",
        "protistan_parasite",
        "sooty_mold",
        "unspecified_pathotroph"
      ) ~ "Pathotroph",
    x %in%
      c(
        "animal-associated",
        "animal_endosymbiont",
        "arbuscular_mycorrhizal",
        "arthropod-associated",
        "ectomycorrhizal",
        "epiphyte",
        "foliar_endophyte",
        "lichenized",
        "moss_symbiont",
        "root_endophyte",
        "unspecified_symbiotroph"
      ) ~ "Symbiotroph",
    is.na(x) | x %in% c("unspecified", "") ~ NA_character_,
    .default = "Other"
  )
}
