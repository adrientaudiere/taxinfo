#' Verify (and fix) scientific names (Genus species) of a phyloseq object.
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-maturing-blue" alt="lifecycle-maturing"></a>
#'
#'  A wrapper of [taxize::gna_verifier()] apply to phyloseq object
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character)
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. the default c("Genus", "Species")).
#' @param data_sources A character or integer vector of Global Names
#'   Architecture (GNA) data source id(s). See [taxize::gna_verifier()]. For
#'   example, 1=Catalogue of Life, 3=ITIS, 5=Index Fungarum, 11=GBIF backbone
#'   and 210=TaxRef.
#'
#'   The choice of `data_sources` matters and should be adapted to the database
#'   queried by the downstream function: aligning names to the same backbone as
#'   the target greatly improves match rates. In particular, use
#'   `data_sources = 11` (GBIF backbone) before the GBIF-based functions
#'   ([tax_gbif_occur_pq()], [tax_iucn_code_pq()], [tax_photos_pq()],
#'   [tax_occur_check_pq()], [tax_ecoregion_occur_pq()]). The data sources are
#'   **not** all refreshed on the same schedule
#'   (<https://verifier.globalnames.org/data_sources>), so no single source is
#'   universally best; a stale source can silently miss recently described taxa
#'   (see `max_age_days`).
#' @param max_age_days (numeric, default `365`) Age threshold, in days, above
#'   which a selected `data_sources` entry triggers an informative message about
#'   its last update date. The check is best-effort and silent when offline.
#' @param all_matches (Logical) See [taxize::gna_verifier()] documentation.
#' @param capitalize (Logical) See [taxize::gna_verifier()] documentation.
#' @param species_group (Logical) See [taxize::gna_verifier()] documentation.
#' @param fuzzy_uninomial (Logical) See [taxize::gna_verifier()] documentation.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided)
#'
#'  - If FALSE, return a cleaned tibble derived from [taxize::gna_verifier()]
#'    output, with columns `submittedName`, `currentName`,
#'    `currentCanonicalSimple` (and `genusEpithet`/`specificEpithet` when
#'    `genus_species_canonical_col = TRUE`, `namePublishedInYear` when
#'    `year_col = TRUE`, authorship columns when `authorship_col = TRUE`),
#'    plus a column `taxa_names_in_phyloseq` with the original taxon name
#'    from the phyloseq object (or `NULL` when `taxnames` is provided
#'    directly).
#'
#'  - If TRUE return a phyloseq object with amended slot `@taxtable`. Cannot be TRUE if `taxnames` is provided.
#'    At least three new columns are added:
#'    - **taxa_name**: The character string sent to gna_verifier (e.g.
#'    `Antrodiella brasiliensis`)
#'    - **currentName**: The current accepted name (resolve the synonym) with
#'      autorities at the end of the binominal name (e.g.
#'      `Trametopsis brasiliensis (Ryvarden & de Meijer) Gomez-Mont. & Robledo)`.
#'    - **currentCanonicalSimple**: The current accepted name without autorities
#'      (e.g. `Trametopsis brasiliensis`, `Russula`).
#'
#'      Other columns can be added depending on the parameters:
#'       `genus_species_canonical_col` (adds "genusEpithet", "specificEpithet",
#'       and "genusSpeciesEpithet"), `year_col`, `authorship`.
#' @param col_prefix A character string to be added as a prefix to the new
#' columns names added to the tax_table slot of the phyloseq object (default: NULL).
#' @param genus_species_canonical_col (logical, default TRUE) If TRUE
#'   three new columns are added along with "currentCanonicalSimple":
#'   "genusEpithet", "specificEpithet" and "genusSpeciesEpithet".
#'   "genusSpeciesEpithet" is identical to "currentCanonicalSimple" but is NA
#'   when "specificEpithet" is NA or empty (i.e. genus-only names are excluded).
#' @param year_col (logical, default TRUE) If TRUE
#'  a new column "namePublishedInYear" is added with the year of publication.
#' @param species_only (logical, default TRUE) If TRUE, `currentCanonicalSimple`
#'   is set to `NA` for uninomial names (i.e. when `matchedCardinality == 1`,
#'   meaning only a genus or higher-rank name was matched, not a proper species
#'   binomial). The `genusEpithet` column is always populated from the matched
#'   name regardless of this setting. The `specificEpithet` column is always
#'   `NA` for uninomials, independently of this parameter.
#' @param authorship_col (logical, default TRUE) If TRUE three new columns are added:
#'  "authorship", "bracketauthorship" and "scientificNameAuthorship".
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param problematic_chars A regex pattern (character string) to detect
#'  characters that are problematic for the GNA Verifier API URL. The API
#'  pastes names pipe-separated into a GET URL path, so characters like
#'  `?` (query-string delimiter), `\\` (escape), `|` (pipe separator),
#'  `#` (fragment), or `&` (parameter separator) corrupt the URL and can
#'  cause a length-mismatch crash in [taxize::gna_verifier()]. Names
#'  containing these characters are reported and, if
#'  `clean_problematic_chars` is `TRUE`, handled before verification.
#'  Set to `NULL` to disable detection. Default: `"[?\\\\#|&]"`.
#' @param clean_problematic_chars (logical, default `FALSE`) If `TRUE`,
#'  cells in the `taxonomic_rank` columns that match `problematic_chars`
#'  are replaced with `NA` (when `physeq` is provided) and matching names
#'  are filtered out (when `taxnames` is provided) before verification.
#'  If `FALSE` (the default), a warning is issued listing the problematic
#'  names but they are sent as-is -- this will likely cause an error in
#'  [taxize::gna_verifier()]. Set to `TRUE` to handle them automatically,
#'  or clean the data upstream (e.g. with [MiscMetabar::simplify_taxo()]).
#' @param force_recompute (logical, default `FALSE`) If `TRUE`, remove
#'  any existing columns in the `tax_table` that would be re-added by
#'  this call (i.e. columns matching `col_prefix` when `col_prefix` is
#'  set, or columns in `new_cols` when `col_prefix` is `NULL`) before
#'  performing the verification. This is useful when re-running
#'  `gna_verifier_pq()` on a phyloseq that already contains result
#'  columns from a previous call. If `FALSE`, existing columns are left
#'  in place, which can cause duplicate-column errors in
#'  `tax_table()` on re-runs.
#' @returns
#'   Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq object
#'   with new columns (see param add_to_phyloseq) in the tax_table slot.
#' @export
#' @author Adrien Taudiere
#'
#' @seealso [taxize::gna_verifier()]
#' @examples
#' \dontrun{
#' df <- gna_verifier_pq(data_fungi, data_sources = 210, add_to_phyloseq = FALSE)
#'
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini, data_sources = 210)
#'
#' data_fungi_cleanNames <- gna_verifier_pq(data_fungi, data_sources = 210)
#'
#' sum(!is.na(data_fungi_cleanNames@tax_table[, "currentName"]))
#' sum(data_fungi_cleanNames@tax_table[, "currentCanonicalSimple"] !=
#'   data_fungi_cleanNames@tax_table[, "taxa_name"], na.rm = TRUE)
#' # 1010 taxa (71% of total) are identified using a currentName including 434
#' # corrected values (correction using synonym disambiguation)
#'
#'
#' tr <- rotl_pq(data_fungi_cleanNames,
#'   taxonomic_rank = "currentCanonicalSimple",
#'   context_name = "Basidiomycetes"
#' )
#'
#' p <- ggtree::ggtree(tr, layout = "roundrect") +
#'   ggtree::geom_nodelab(hjust = 1, vjust = -1.2, size = 2) +
#'   ggtree::geom_tiplab(size = 2)
#'
#' p + xlim(0, max(p$data$x) + 1)
#'
#'
#' psmelt(data_fungi_mini_cleanNames) |>
#'   filter(Abundance > 0) |>
#'   mutate(namePublishedInYear = as.numeric(namePublishedInYear)) |>
#'   pull(namePublishedInYear) |>
#'   hist(breaks = 100)
#'
#'
#' # Does the fungal species discovered more recently tend to be found at
#' # greater heights in the tree?
#' psmelt(data_fungi_mini_cleanNames) |>
#'   filter(Abundance > 0) |>
#'   group_by(Height) |>
#'   mutate(namePublishedInYear = as.numeric(namePublishedInYear)) |>
#'   ggstatsplot::ggbetweenstats("Height", "namePublishedInYear")
#' }
#' @details
#' This function is mainly a wrapper of the work of others.
#'   Please cite `taxize` package.
gna_verifier_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = c("Genus", "Species"),
  data_sources = c(1, 12),
  all_matches = FALSE,
  capitalize = FALSE,
  species_group = FALSE,
  fuzzy_uninomial = FALSE,
  verbose = TRUE,
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  genus_species_canonical_col = TRUE,
  year_col = TRUE,
  authorship_col = TRUE,
  discard_NA = TRUE,
  problematic_chars = "[?\\\\#|&]",
  clean_problematic_chars = FALSE,
  force_recompute = FALSE,
  species_only = TRUE,
  max_age_days = 365
) {
  resolved <- resolve_taxa_input(
    physeq = physeq,
    taxnames = taxnames,
    add_to_phyloseq = add_to_phyloseq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = FALSE,
    discard_NA = discard_NA
  )
  taxnames <- resolved$taxnames
  add_to_phyloseq <- resolved$add_to_phyloseq

  if (verbose) {
    check_data_sources_freshness(data_sources, max_age_days = max_age_days)
  }

  # Detect and handle problematic characters that break the GNA Verifier URL
  if (!is.null(problematic_chars)) {
    problematic <- grepl(problematic_chars, taxnames)
    if (any(problematic)) {
      n_problematic <- sum(problematic)
      examples <- head(taxnames[problematic], 5)
      if (clean_problematic_chars) {
        cli::cli_warn(c(
          "!" = "{n_problematic} taxonomic name(s) contain characters problematic for the GNA Verifier API.",
          "i" = "Pattern: {.val {problematic_chars}}",
          "i" = "Examples: {.val {examples}}",
          "i" = "They will be replaced with NA before verification."
        ))
        if (!is.null(physeq)) {
          for (col in taxonomic_rank) {
            if (col %in% colnames(physeq@tax_table)) {
              vals <- as.character(physeq@tax_table[, col])
              vals[grepl(problematic_chars, vals)] <- NA_character_
              physeq@tax_table[, col] <- vals
            }
          }
          taxnames <- taxonomic_rank_to_taxnames(
            physeq = physeq,
            taxonomic_rank = taxonomic_rank,
            discard_genus_alone = FALSE,
            discard_NA = discard_NA
          )
        } else {
          taxnames <- taxnames[!problematic]
        }
      } else {
        cli::cli_warn(c(
          "!" = "{n_problematic} taxonomic name(s) contain characters problematic for the GNA Verifier API.",
          "i" = "Pattern: {.val {problematic_chars}}",
          "i" = "Examples: {.val {examples}}",
          "i" = "Set {.code clean_problematic_chars = TRUE} to handle them automatically.",
          "i" = "Proceeding as-is may cause an error in {.fn taxize::gna_verifier}."
        ))
      }
    }
  }

  # Determine column names that will be added
  new_cols <- c("submittedName", "currentName", "currentCanonicalSimple")
  if (genus_species_canonical_col) {
    new_cols <- c(
      new_cols,
      "genusEpithet",
      "specificEpithet",
      "genusSpeciesEpithet"
    )
  }

  # Optionally remove existing result columns so verification can be re-run.
  # Any remaining collision is handled by augment_tax_table() below.
  if (add_to_phyloseq && force_recompute) {
    prefixed_new_cols <- paste0(col_prefix, new_cols)
    common_cols <- intersect(prefixed_new_cols, colnames(physeq@tax_table))
    if (length(common_cols) > 0) {
      cli::cli_alert_info(
        "Removing {.val {length(common_cols)}} existing column(s) before re-adding: {.val {head(common_cols, 5)}}"
      )
      tax_mat <- as(physeq@tax_table, "matrix")
      tax_mat <- tax_mat[,
        !(colnames(tax_mat) %in% common_cols),
        drop = FALSE
      ]
      physeq@tax_table <- tax_table(tax_mat)
    }
  }

  slice_taxnames <- if (length(taxnames) > 50) {
    # gna_verifier can show bugs when handle more than 49 names at a time
    split(taxnames, ceiling(seq_along(taxnames) / 49))
  } else {
    list(taxnames)
  }

  .call_gna <- function(x) {
    taxize::gna_verifier(
      x,
      data_sources = data_sources,
      all_matches = all_matches,
      capitalize = capitalize,
      species_group = species_group,
      fuzzy_uninomial = fuzzy_uninomial,
      output_type = "table"
    )
  }

  res_verifier <- bind_rows(lapply(slice_taxnames, function(x) {
    tryCatch(
      .call_gna(x),
      error = function(e) {
        # taxize::gna_verifier can fail when the API returns fewer records than
        # expected (mismatched names vector). Fall back to one-by-one queries.
        cli::cli_warn(c(
          "!" = "Batch GNA query failed ({conditionMessage(e)}), retrying one name at a time."
        ))
        bind_rows(lapply(x, \(name) {
          tryCatch(.call_gna(name), error = \(e2) NULL)
        }))
      }
    )
  }))

  res_verifier_clean <-
    res_verifier |>
    select(
      submittedName,
      currentName,
      currentCanonicalSimple,
      matchedCardinality
    ) |>
    distinct(submittedName, .keep_all = TRUE)

  if (genus_species_canonical_col) {
    res_verifier_clean <- res_verifier_clean |>
      mutate(
        genusEpithet = stringr::str_split_i(currentCanonicalSimple, " ", 1),
        specificEpithet = if_else(
          !is.na(matchedCardinality) & matchedCardinality == 1,
          NA_character_,
          stringr::str_split_i(currentCanonicalSimple, " ", 2)
        ),
        genusSpeciesEpithet = ifelse(
          is.na(.data$specificEpithet) | .data$specificEpithet == "",
          NA_character_,
          .data$currentCanonicalSimple
        )
      )
  }

  if (species_only) {
    res_verifier_clean <- res_verifier_clean |>
      mutate(
        currentCanonicalSimple = if_else(
          !is.na(matchedCardinality) & matchedCardinality == 1,
          NA_character_,
          currentCanonicalSimple
        )
      )
  }

  res_verifier_clean <- res_verifier_clean |> select(-matchedCardinality)

  if (year_col) {
    res_verifier_clean$namePublishedInYear <- rgbif::name_parse(
      res_verifier_clean$currentName
    )$year
  }

  if (authorship_col) {
    res_verifier_clean <- res_verifier_clean |>
      mutate(
        authorship = rgbif::name_parse(currentName)$authorship,
        bracketauthorship = rgbif::name_parse(
          currentName
        )$bracketauthorship %||%
          NA,
        scientificNameAuthorship = ifelse(
          is.na(bracketauthorship),
          authorship,
          paste0("(", bracketauthorship, ") ", authorship)
        )
      )
  }

  if (add_to_phyloseq) {
    new_physeq <- augment_tax_table(
      physeq,
      res_verifier_clean,
      taxonomic_rank = taxonomic_rank,
      info_key = "submittedName",
      col_prefix = col_prefix,
      default_prefix = "gna_"
    )

    taxtab_new <- new_physeq@tax_table |>
      as.data.frame() |>
      tibble()

    if (verbose) {
      total_taxa <- ntaxa(physeq)
      submitted_taxa <- sum(taxtab_new$taxa_name != "")
      genus_only_taxa <- sum(
        !grepl(" ", taxtab_new$taxa_name) & taxtab_new$taxa_name != ""
      )
      total_matches <- sum(
        res_verifier$taxonomicStatus %in% c("Synonym", "Accepted")
      )
      synonyms <- sum(res_verifier$taxonomicStatus == "Synonym", na.rm = TRUE)
      uninomial_synonyms <- sum(
        res_verifier$matchedCardinality == 1 &
          res_verifier$taxonomicStatus == "Synonym",
        na.rm = TRUE
      )
      accepted_names <- sum(
        res_verifier$taxonomicStatus == "Accepted",
        na.rm = TRUE
      )
      uninomial_accepted <- sum(
        res_verifier$matchedCardinality == 1 &
          res_verifier$taxonomicStatus == "Accepted",
        na.rm = TRUE
      )

      species_only_msg <- if (species_only && uninomial_accepted > 0) {
        c(
          "i" = "{.val {uninomial_accepted}} uninomial accepted name(s) have {.code currentCanonicalSimple} set to {.val NA} ({.arg species_only} = TRUE)"
        )
      }

      cli::cli_bullets(c(
        "v" = "GNA verification summary:",
        "*" = "Total taxa in phyloseq: {.val {total_taxa}}",
        "*" = "Taxa submitted for verification: {.val {submitted_taxa}}",
        "*" = "Genus-level only taxa: {.val {genus_only_taxa}}",
        "*" = "Total matches found: {.val {total_matches}}",
        "*" = "Synonyms: {.val {synonyms}} (including {.val {uninomial_synonyms}} uninomial)",
        "*" = "Accepted names: {.val {accepted_names}} (including {.val {uninomial_accepted}} uninomial)",
        species_only_msg
      ))
    }
    return(new_physeq)
  } else {
    if (verbose) {
      total_matches <- sum(
        res_verifier$taxonomicStatus %in% c("Synonym", "Accepted")
      )
      synonyms <- sum(
        res_verifier$taxonomicStatus == "Synonym",
        na.rm = TRUE
      )
      uninomial_synonyms <- sum(
        res_verifier$matchedCardinality == 1 &
          res_verifier$taxonomicStatus == "Synonym",
        na.rm = TRUE
      )
      accepted_names <- sum(
        res_verifier$taxonomicStatus == "Accepted",
        na.rm = TRUE
      )
      uninomial_accepted <- sum(
        res_verifier$matchedCardinality == 1 &
          res_verifier$taxonomicStatus == "Accepted",
        na.rm = TRUE
      )

      species_only_msg <- if (species_only && uninomial_accepted > 0) {
        c(
          "i" = "{.val {uninomial_accepted}} uninomial accepted name(s) have {.code currentCanonicalSimple} set to {.val NA} ({.arg species_only} = TRUE)"
        )
      }

      cli::cli_bullets(c(
        "v" = "GNA verification summary:",
        "*" = "Taxa submitted for verification: {.val {length(taxnames)}}",
        "*" = "Total matches found: {.val {total_matches}}",
        "*" = "Synonyms: {.val {synonyms}} (including {.val {uninomial_synonyms}} uninomial)",
        "*" = "Accepted names: {.val {accepted_names}} (including {.val {uninomial_accepted}} uninomial)",
        species_only_msg
      ))
    }
    res_verifier_clean$taxa_names_in_phyloseq <- if (
      !is.null(names(taxnames))
    ) {
      names(taxnames)
    } else {
      taxnames
    }

    # Apply col_prefix to returned tibble if specified
    if (!is.null(col_prefix)) {
      res_verifier_clean <- res_verifier_clean |>
        rename_with(~ paste0(col_prefix, .), .cols = -taxa_names_in_phyloseq)
    }

    return(res_verifier_clean)
  }
}
