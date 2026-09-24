#' Add BactoTraits functional traits to a phyloseq object
#'
#' @description
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Augments the `tax_table` slot of a bacterial (or archaeal) phyloseq object
#' with functional traits from the BactoTraits database (Laderriere et al.
#' 2026, <https://doi.org/10.1038/s41597-026-06652-2>; data at
#' <https://doi.org/10.24396/ORDAR-182>, CC-BY). BactoTraits describes 31
#' functional traits (morphology, physiology, metabolism, genome properties)
#' for 97 721 bacterial strains as *fuzzy-coded* modality columns: one column
#' per `trait_modality` pair (e.g. `motility_yes`, `motility_no`) holding the
#' affinity score of the strain (or taxon) for that modality, so that the
#' modalities of a trait sum to one.
#'
#' Matching is taxon-first with a rank fallback (as in the original BactoTraits
#' scripts): the most precise level that can be assigned wins
#' (`species` > `genus` > `family` > `order` > `class` > `phylum`), and the
#' trait profile of coarser levels is the strain-weighted mean of the genus
#' rows of that level. The column `trait_level` records where the profile
#' comes from.
#'
#' @param physeq (required) A phyloseq object with bacterial taxonomic ranks
#'  in its `tax_table`.
#' @param file_name_species,file_name_genus (Character, default `NULL`) Paths
#'  to the BactoTraits species-level and genus-level CSV files
#'  (`BACTOTRAITS_DATASET_*_SPECIESLVL.csv` and `*_GENUSLVL.csv`). When `NULL`
#'  (and the corresponding levels are requested), the files are downloaded
#'  once from <https://doi.org/10.24396/ORDAR-182> and cached in `cache_dir`.
#'  Only the genus-level file is used for levels coarser than `genus`.
#' @param taxonomic_rank (Character vector of 6, default
#'  `c("Phylum", "Class", "Order", "Family", "Genus", "Species")`) The columns
#'  of the `tax_table` slot holding the six BactoTraits ranks, from the
#'  highest (first) to the most precise (last). Their names need not match the
#'  BactoTraits column names: they are matched positionally.
#' @param level (Character vector, default all six) Matching levels allowed.
#'  Their order does not matter: the most precise allowed level that matches
#'  always wins (`species` > `genus` > `family` > `order` > `class` >
#'  `phylum`). Use e.g. `c("species", "genus")` to forbid coarser
#'  aggregation.
#' @param merge_modalities (Logical, default `TRUE`) If `TRUE`, every
#'  *binary* trait (two modalities, e.g. `motility_yes` / `motility_no`) is
#'  collapsed to a single column named after the trait (`motility`) with
#'  values `TRUE`, `FALSE` or a probability (see Details). Traits with more
#'  than two modalities (e.g. `cell_shape`) cannot be encoded in one value and
#'  keep their raw modality columns.
#' @param positive_modalities (Character vector,
#'  default `c("yes", "positive", "present", "true")`) Modality names that
#'  define the "positive" side of a binary trait. The first matching modality
#'  is the one encoded in the merged column; when none matches, the first
#'  modality in column order is used.
#' @param raw_cols (Logical, default `FALSE`) If `TRUE`, keep (in addition to
#'  the merged columns when `merge_modalities = TRUE`) every raw
#'  `trait_modality` column. Always `TRUE` when
#'  `merge_modalities = FALSE`.
#' @param col_prefix (Character, default `"bt_"`) Prefix added to every new
#'  column of the `tax_table`.
#' @param add_to_phyloseq (Logical, default `TRUE`) If `TRUE`, return a new
#'  phyloseq object with new columns in the `tax_table` slot. If `FALSE`,
#'  return a tibble of the augmented `tax_table`.
#' @param cache_dir (Character) Directory used to cache the downloaded
#'  BactoTraits files. Defaults to [tools::R_user_dir()].
#' @param refresh (Logical, default `FALSE`) If `TRUE`, re-download the
#'  BactoTraits files even if they are already cached.
#' @param verbose (Logical, default `TRUE`) If `TRUE`, prompt some messages.
#'
#' @details
#' **Merged binary columns.** For a binary trait the reference modality is the
#' first one whose modality name is in `positive_modalities` (e.g. `yes` for
#' `motility`, `positive` for `gram_stain`), falling back to the first modality
#' in column order (`chemotroph` for `energy_source`, `autotroph` for
#' `carbon_source`, ...). The merged column encodes the affinity `a` of the
#' reference modality as `TRUE` when `a == 1` (the taxon is assigned
#' exclusively to that modality), `FALSE` when `a == 0`, and the affinity
#' score itself (a fuzzy-coded probability in `]0, 1[`) otherwise. As
#' everywhere in the `tax_*_pq` family, columns are character once they reach
#' the `tax_table` matrix.
#'
#' **Rank fallback.** A taxon whose species is absent from the species-level
#' file falls back to its genus (genus-level file), then to the strain-weighted
#' mean profile of its family, order, class or phylum (computed from the
#' genus-level rows). The profile comes *entirely* from the most precise
#' assignable level: levels are never mixed column-wise. Use `trait_level` to
#' filter on the match level and `n_strains` (the number of BacDive strains
#' behind the profile) to filter on data support.
#'
#' **Species names.** The species-level key is the binomial
#' `"<Genus> <epithet>"`. The `Species` rank column may hold that binomial, a
#' bare epithet (glued to the genus), or the genus directly followed by the
#' epithet without a space, as in `GlobalPatterns`
#' (`"Sulfolobusacidocaldarius"` for genus `"Sulfolobus"`, read as
#' `"Sulfolobus acidocaldarius"`). Other values (e.g. clone names such as
#' `"SCA1145"`) find no species match and fall back to coarser levels.
#'
#' @returns Either an updated phyloseq object (when `add_to_phyloseq = TRUE`)
#'  or a tibble of the augmented `tax_table`, with the new columns
#'  `bt_trait_level`, `bt_n_strains`, one merged column per binary trait
#'  (`bt_gram_stain`, `bt_motility`, `bt_pigmentation_production`,
#'  `bt_forms_multicellular_complex`, `bt_spore_formation`,
#'  `bt_energy_source`, `bt_electron_donnor`, `bt_carbon_source`) and the raw
#'  modality columns of the non-binary traits (e.g.
#'  `bt_cell_shape_coccus-shaped`).
#'
#' @author Adrien Taudiere
#' @references
#' Laderriere, V., Usseglio-Polatera, F., Maunoury-Danger, F. & Cebron, A.
#' (2026). BactoTraits: a trait database for exploring functional diversity of
#' bacterial communities. *Scientific Data* 13, 337.
#' \doi{10.1038/s41597-026-06652-2}
#'
#' @seealso [tax_metatraits_pq()], [tax_faprotax_pq()], [tax_info_pq()]
#' @examples
#' \donttest{
#' data(GlobalPatterns, package = "phyloseq")
#'
#' # The bundled mini files keep the example fast and fully offline
#' res <- tax_bactotraits_pq(GlobalPatterns,
#'   file_name_species = system.file("extdata", "bactotraits_species_mini.csv",
#'     package = "taxinfo"
#'   ),
#'   file_name_genus = system.file("extdata", "bactotraits_genus_mini.csv",
#'     package = "taxinfo"
#'   ),
#'   verbose = FALSE
#' )
#' table(res@tax_table[, "bt_trait_level"], useNA = "ifany")
#' table(res@tax_table[, "bt_motility"], useNA = "ifany")
#' }
#'
#' \dontrun{
#' # Full BactoTraits database: the species- and genus-level files are
#' # downloaded from https://doi.org/10.24396/ORDAR-182 once, then cached
#' res_full <- tax_bactotraits_pq(GlobalPatterns)
#'
#' # Keep the raw modality columns of the binary traits as well
#' res_raw <- tax_bactotraits_pq(GlobalPatterns, raw_cols = TRUE)
#' }
#' @export
tax_bactotraits_pq <- function(
  physeq,
  file_name_species = NULL,
  file_name_genus = NULL,
  taxonomic_rank = c(
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species"
  ),
  level = c(
    "species",
    "genus",
    "family",
    "order",
    "class",
    "phylum"
  ),
  merge_modalities = TRUE,
  positive_modalities = c("yes", "positive", "present", "true"),
  raw_cols = FALSE,
  col_prefix = "bt_",
  add_to_phyloseq = TRUE,
  cache_dir = tools::R_user_dir("taxinfo", "cache"),
  refresh = FALSE,
  verbose = TRUE
) {
  if (is.null(physeq) || !methods::is(physeq, "phyloseq")) {
    cli::cli_abort("{.arg physeq} must be a {.cls phyloseq} object.")
  }
  level <- match.arg(
    level,
    c("species", "genus", "family", "order", "class", "phylum"),
    several.ok = TRUE
  )

  if (length(taxonomic_rank) != 6) {
    cli::cli_abort(
      "{.arg taxonomic_rank} must hold the 6 BactoTraits ranks (phylum to species)."
    )
  }
  missing_ranks <- setdiff(taxonomic_rank, colnames(physeq@tax_table))
  if (length(missing_ranks) > 0) {
    cli::cli_abort(
      "Rank columns missing from the {.field tax_table}: {.val {missing_ranks}}"
    )
  }

  # Which BactoTraits files are needed for the requested levels?
  need_species <- "species" %in% level
  need_genus <- any(c("genus", "family", "order", "class", "phylum") %in% level)

  if (need_species) {
    file_name_species <- bt_resolve_file(
      file_name_species,
      kind = "species",
      cache_dir = cache_dir,
      refresh = refresh,
      verbose = verbose
    )
  }
  if (need_genus) {
    file_name_genus <- bt_resolve_file(
      file_name_genus,
      kind = "genus",
      cache_dir = cache_dir,
      refresh = refresh,
      verbose = verbose
    )
  }

  # Per-level profile tables (one row per key, trait columns in `mod_cols`)
  sp_raw <- if (need_species) {
    bt_read_bactotraits(file_name_species)
  } else {
    NULL
  }
  gen_raw <- if (need_genus) {
    bt_read_bactotraits(file_name_genus)
  } else {
    NULL
  }
  mod_cols <- setdiff(
    colnames(if (!is.null(sp_raw)) sp_raw else gen_raw),
    c("Total_BacDive_ids", unname(BT_RANK_COLS))
  )
  sp_tbl <- if (need_species) {
    bt_profile_table(sp_raw, "Species", mod_cols)
  } else {
    NULL
  }
  gen_tbl <- if ("genus" %in% level) {
    bt_profile_table(gen_raw, "Genus", mod_cols)
  } else {
    NULL
  }

  coarse_tbls <- list()
  for (lv in intersect(c("phylum", "class", "order", "family"), level)) {
    coarse_tbls[[lv]] <- bt_profile_table(
      gen_raw,
      BT_RANK_COLS[[lv]],
      mod_cols = mod_cols
    )
  }

  # Per-taxon fill: coarsest level first, finer levels replace the profile
  # entirely (the most precise assignable level wins, as in the original
  # BactoTraits scripts -- no column-wise mixing between levels).
  tax_df <- as.data.frame(unclass(physeq@tax_table), stringsAsFactors = FALSE)
  n_taxa <- nrow(tax_df)
  rank_col <- stats::setNames(taxonomic_rank, names(BT_RANK_COLS))

  result <- as.data.frame(
    matrix(
      NA_real_,
      nrow = n_taxa,
      ncol = length(mod_cols),
      dimnames = list(NULL, mod_cols)
    ),
    stringsAsFactors = FALSE
  )
  n_strains <- rep(NA_real_, n_taxa)
  trait_level <- rep(NA_character_, n_taxa)

  fill_steps <- list()
  for (lv in c("phylum", "class", "order", "family")) {
    if (!is.null(coarse_tbls[[lv]])) {
      fill_steps[[length(fill_steps) + 1]] <- list(
        tbl = coarse_tbls[[lv]],
        keys = fe_clean_name(tax_df[[rank_col[[lv]]]]),
        lv = lv
      )
    }
  }
  if (!is.null(gen_tbl)) {
    fill_steps[[length(fill_steps) + 1]] <- list(
      tbl = gen_tbl,
      keys = fe_clean_name(tax_df[[rank_col[["genus"]]]]),
      lv = "genus"
    )
  }
  if (!is.null(sp_tbl)) {
    fill_steps[[length(fill_steps) + 1]] <- list(
      tbl = sp_tbl,
      keys = bt_species_key(
        fe_clean_name(tax_df[[rank_col[["genus"]]]]),
        fe_clean_name(tax_df[[rank_col[["species"]]]])
      ),
      lv = "species"
    )
  }

  for (step in fill_steps) {
    m <- match(step$keys, step$tbl$taxa_name)
    hit <- !is.na(m)
    if (!any(hit)) {
      next
    }
    for (col in mod_cols) {
      result[[col]][hit] <- step$tbl[[col]][m[hit]]
    }
    n_strains[hit] <- step$tbl$n_strains[m[hit]]
    trait_level[hit] <- step$lv
  }

  # Assemble the summary table, keyed in the query namespace expected by
  # augment_tax_table() (see the key invariant documented there).
  query_key <- taxnames_from_rank(
    physeq@tax_table,
    taxonomic_rank,
    clean = TRUE
  )

  groups <- bt_trait_groups(colnames(result))
  binary_traits <- names(groups)[lengths(groups) == 2]
  keep_raw <- raw_cols || !merge_modalities

  out <- tibble::tibble(
    trait_level = trait_level,
    n_strains = as.character(n_strains)
  )

  if (merge_modalities) {
    for (tr in binary_traits) {
      mods <- groups[[tr]]
      ref <- bt_pick_reference(mods, tr, positive_modalities)
      out[[tr]] <- bt_recode_affinity(result[[ref]])
    }
  }
  raw_cols_to_keep <- if (keep_raw) {
    mod_cols
  } else {
    mod_cols[!mod_cols %in% unlist(groups[binary_traits], use.names = FALSE)]
  }
  for (col in raw_cols_to_keep) {
    # as.character() here (not at the as.matrix() step): as.matrix() on a
    # mixed data frame formats numerics with padding ("0.50" for 0.5).
    out[[col]] <- as.character(result[[col]])
  }

  info_tbl <- dplyr::bind_cols(
    tibble::tibble(taxa_name = query_key),
    out
  ) |>
    dplyr::filter(!duplicated(.data$taxa_name))

  if (verbose) {
    cli::cli_alert_success(
      "Added {.val {ncol(out)}} BactoTraits column{?s} for {.val {sum(!is.na(trait_level))}}/{.val {n_taxa}} taxa ({.val {sum(trait_level == 'species', na.rm = TRUE)}} at species level)."
    )
  }

  new_physeq <- augment_tax_table(
    physeq = physeq,
    info_tbl = info_tbl,
    taxonomic_rank = taxonomic_rank,
    col_prefix = col_prefix,
    keep_key = FALSE
  )

  if (add_to_phyloseq) {
    return(new_physeq)
  } else {
    return(tibble::as_tibble(as.data.frame(new_physeq@tax_table)))
  }
}

# Internal helpers -------------------------------------------------------------

#' The 31 BactoTraits trait names (prefix of every `trait_modality` column)
#'
#' Kept as an internal constant: splitting `trait_modality` column names at the
#' last underscore is not possible (`cell_length_<=_0.9` is one trait with a
#' range modality), so group membership is decided by longest-prefix matching
#' against this authoritative list.
#' @noRd
BT_TRAITS <- c(
  "gram_stain",
  "cell_length",
  "cell_width",
  "cell_shape",
  "motility",
  "flagellum_arrangement",
  "pigmentation_production",
  "colony_color",
  "forms_multicellular_complex",
  "temperature_opt",
  "temperature_preference",
  "temperature_range",
  "temperature_delta",
  "pH_opt",
  "pH_preference",
  "pH_range",
  "pH_delta",
  "spore_formation",
  "halophily_opt",
  "halophily_preference",
  "halophily_range",
  "halophily_delta",
  "oxygen_tolerance",
  "antibiotic_resistance_frequency",
  "energy_source",
  "electron_donnor",
  "carbon_source",
  "biosafety_level",
  "gc_content",
  "rRNA16S_gene_copies",
  "estimated_genome_size"
)

#' Rank level -> BactoTraits column name (coarsest first)
#' @noRd
BT_RANK_COLS <- c(
  phylum = "Phylum",
  class = "Class",
  order = "Order",
  family = "Family",
  genus = "Genus",
  species = "Species"
)

#' BactoTraits download URLs (CC-BY, https://doi.org/10.24396/ORDAR-182)
#' @noRd
BT_URLS <- c(
  species = paste0(
    "https://ordar.otelo.univ-lorraine.fr/files/ORDAR-182/",
    "BACTOTRAITS_DATASET_2026-01-28_SPECIESLVL.csv"
  ),
  genus = paste0(
    "https://ordar.otelo.univ-lorraine.fr/files/ORDAR-182/",
    "BACTOTRAITS_DATASET_2026-01-28_GENUSLVL.csv"
  )
)

#' Resolve a BactoTraits file: local path or download-once into `cache_dir`
#' @noRd
bt_resolve_file <- function(file_name, kind, cache_dir, refresh, verbose) {
  if (!is.null(file_name)) {
    if (!file.exists(file_name)) {
      cli::cli_abort("The file path {.path {file_name}} does not exist")
    }
    return(file_name)
  }
  fname <- basename(BT_URLS[[kind]])
  dest <- file.path(cache_dir, fname)
  if (refresh || !file.exists(dest)) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    if (verbose) {
      cli::cli_inform(c(
        "i" = "Downloading the BactoTraits {.val {kind}}-level file to {.path {cache_dir}}.",
        " " = "This happens once; subsequent calls reuse the cached file."
      ))
    }
    utils::download.file(BT_URLS[[kind]], dest, mode = "wb", quiet = !verbose)
  }
  dest
}

#' Read a BactoTraits CSV file
#'
#' The distributed files use `;` as separator and a decimal comma.
#' @noRd
bt_read_bactotraits <- function(file_name) {
  utils::read.csv(
    file_name,
    sep = ";",
    dec = ",",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Group `trait_modality` columns by trait (longest-prefix matching)
#'
#' @returns A named list, one element per trait present in `cols`, holding the
#'  modality columns of that trait in column order. Columns that match no
#'  trait prefix are ignored.
#' @noRd
bt_trait_groups <- function(cols) {
  traits <- BT_TRAITS[order(nchar(BT_TRAITS), decreasing = TRUE)]
  hit <- vapply(
    cols,
    function(col) {
      found <- traits[startsWith(col, paste0(traits, "_"))]
      if (length(found) == 0) NA_character_ else found[[1]]
    },
    character(1)
  )
  split(cols[!is.na(hit)], hit[!is.na(hit)])
}

#' Pick the reference modality of a binary trait
#'
#' First modality whose name is in `positive_modalities`, else the first
#' modality in column order.
#' @noRd
bt_pick_reference <- function(mod_cols, trait, positive_modalities) {
  mod_names <- substring(mod_cols, nchar(trait) + 2)
  positive <- tolower(mod_names) %in% tolower(positive_modalities)
  if (any(positive)) {
    mod_cols[which(positive)[1]]
  } else {
    mod_cols[1]
  }
}

#' Recode an affinity score as TRUE / FALSE / probability
#'
#' `1` becomes `"TRUE"` (exclusive assignment to the reference modality), `0`
#' becomes `"FALSE"`, and any fuzzy-coded affinity in between is kept as its
#' character representation.
#' @noRd
bt_recode_affinity <- function(x) {
  out <- as.character(x)
  out[!is.na(x) & x >= 1 - 1e-9] <- "TRUE"
  out[!is.na(x) & x <= 1e-9] <- "FALSE"
  out
}

#' Strain-weighted mean robust to missing values
#' @noRd
bt_wmean <- function(x, w) {
  ok <- !is.na(x)
  if (!any(ok)) {
    return(NA_real_)
  }
  if (is.null(w) || all(is.na(w[ok]))) {
    return(mean(x[ok]))
  }
  stats::weighted.mean(x[ok], w[ok])
}

#' Aggregate BactoTraits rows per key into one strain-weighted profile
#'
#' @param df A BactoTraits data frame (species- or genus-level file).
#' @param key_col The column holding the taxonomic key (`"Species"` for
#'  binomials, `"Genus"`, `"Family"`, ...).
#' @param mod_cols The modality columns to average; defaults to every
#'  `trait_modality` column of `df`.
#' @returns A tibble with `taxa_name`, `n_strains` (summed BacDive strain
#'  counts) and one strain-weighted mean column per modality.
#' @noRd
bt_profile_table <- function(df, key_col, mod_cols = NULL) {
  if (is.null(mod_cols)) {
    mod_cols <- setdiff(
      colnames(df),
      c("Total_BacDive_ids", unname(BT_RANK_COLS))
    )
  }
  w <- as.numeric(df[["Total_BacDive_ids"]])
  keys <- fe_clean_name(df[[key_col]])
  keep <- !is.na(keys)
  keys <- keys[keep]
  w <- w[keep]

  tibble::as_tibble(df[keep, mod_cols, drop = FALSE]) |>
    dplyr::mutate(.key = keys, .w = w) |>
    dplyr::group_by(.data$.key) |>
    dplyr::summarise(
      n_strains = sum(.data$.w, na.rm = TRUE),
      dplyr::across(
        dplyr::all_of(mod_cols),
        ~ bt_wmean(as.numeric(.x), .data$.w)
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(taxa_name = ".key")
}

#' Rebuild a "<genus> <epithet>" species key
#'
#' BactoTraits species keys are binomials. When the phyloseq `Species` column
#' already holds a binomial it is used as is; when it holds the genus directly
#' followed by the epithet without a space (e.g. GlobalPatterns'
#' `"Sulfolobusacidocaldarius"` for genus `"Sulfolobus"`) the space is
#' restored; when it holds a bare epithet it is glued to the (already
#' cleaned) genus.
#' @noRd
bt_species_key <- function(genus_key, species) {
  out <- rep(NA_character_, length(species))
  valid <- !is.na(species) & !is.na(genus_key)
  starts_with_genus <- valid &
    startsWith(species, paste0(genus_key, " "))
  glued <- valid &
    !starts_with_genus &
    startsWith(species, genus_key) &
    grepl("^[a-z]", substring(species, nchar(genus_key) + 1))
  simple <- valid & !starts_with_genus & !glued

  out[starts_with_genus] <- species[starts_with_genus]
  out[glued] <- paste(
    genus_key[glued],
    substring(species[glued], nchar(genus_key[glued]) + 1)
  )
  out[simple] <- paste(genus_key[simple], species[simple])
  out
}
