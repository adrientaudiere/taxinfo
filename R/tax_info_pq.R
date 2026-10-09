#' Get information from a custom csv file using taxonomic names present in a
#' phyloseq object
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-maturing-blue" alt="lifecycle-maturing"></a>
#'
#'  A function to add information from a custom csv file (e.g. FungalTraits,
#'  Taxref, ...) to the tax_table slot of a phyloseq object by joining
#'  taxonomic names from phyloseq object (column `taxonomic_rank`) with a
#'  column of the csv file (`csv_taxonomic_rank`) containing the correspondant
#'  taxonomic names. Be careful that the taxonomic names in the csv file must
#'  match exactly the taxonomic names in the phyloseq object. For example, if
#'  the taxonomic names in the phyloseq object are in the form "Genus species"
#'  the taxonomic names in the csv file must be in the same form
#'  (not "Genus_species" or "Genus Species Author"...).
#'
#'  Note that the csv file need to be in a wide-format, i.e. one line for each
#'  distinct value in the `csv_taxonomic_rank` columns. You may want to transform
#'  your data.frame using [tidyr::tidyr::pivot_wider()] fonctions prior to write it in
#'  a new file.
#'
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param file_name (required) A file path to your csv file.
#' @param csv_taxonomic_rank (required) The name of the column in your csv file
#'  containing the taxonomic names. Must match the taxonomic_rank of the phyloseq.
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided)
#'  If TRUE, add new column(s) in the tax_table of the phyloseq object.
#'  Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#'  Cannot be TRUE if `taxnames` is provided.
#' @param col_prefix A character string to be added as a prefix to the new
#' columns names added to the tax_table slot of the phyloseq object. Required
#' when a column of the csv file already exists in the tax_table (e.g.
#' `Phylum`): the function aborts otherwise.
#' @param use_duck_db (logical, default FALSE) If TRUE, use duckdb to read
#'  only the rows of the csv file matching the taxonomic names, instead of
#'  reading the whole file in memory. Useful for large csv files.
#' @param csv_cols_select A character vector of the column names to select in the csv file.
#' @param sep the field separator character. See [utils::read.csv()].
#' @param dec the field separator character. See [utils::read.csv()].
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#' object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#' The new columns (the `csv_taxonomic_rank` column, `NA` for taxa without a
#' match, and the `csv_cols_select` columns, all prefixed by `col_prefix`)
#' are appended after the existing columns of the tax_table, which are left
#' untouched (including a `taxa_name` column). When `taxnames` is provided,
#' the tibble has one row per name, starting with a `taxa_name` column. When
#' a value of `csv_taxonomic_rank` is duplicated in the csv file, only its
#' first row is used.
#' @author Adrien Taudiere
#' @export
#'
#' @examples
#' \dontrun{
#' data_fungi_cleanNames <- gna_verifier_pq(data_fungi,
#'   data_sources = 210
#' )
#'
#' # FUNGAL TRAITS example
#' # --------------------
#' fungal_traits <- system.file("extdata", "fun_trait_mini.csv",
#'   package = "taxinfo") # minidataset for testing
#' # fungal_traits <- system.file("extdata", "fungal_traits.csv", package = "taxinfo")
#' fg_traits <- tax_info_pq(data_fungi_cleanNames,
#'   taxonomic_rank = "genusEpithet",
#'   file_name = fungal_traits,
#'   csv_taxonomic_rank = "GENUS",
#'   col_prefix = "ft_",
#'   sep = "\t",
#'   add_to_phyloseq = FALSE
#' )
#'
#' table(fg_traits$ft_primary_lifestyle, fg_traits$Guild) |>
#'   as.data.frame() |>
#'   filter(Freq > 0) |>
#'   arrange(desc(Freq)) |>
#'   head()
#'
#' # TAXREF example
#' # --------------------
#' TAXREFv18_fungi <- system.file("extdata", "TAXREFv18_fungi.csv", package = "taxinfo")
#'
#' res_with_R <- tax_info_pq(data_fungi_cleanNames,
#'   file_name = TAXREFv18_fungi,
#'   csv_taxonomic_rank = "NOM_VALIDE_SIMPLE",
#'   col_prefix = "taxref_"
#' )
#' res_with_duckDB <- tax_info_pq(
#'   data_fungi_cleanNames,
#'   file_name = TAXREFv18_fungi,
#'   csv_taxonomic_rank = "NOM_VALIDE_SIMPLE",
#'   use_duck_db = TRUE,
#'   add_to_phyloseq = FALSE,
#'   col_prefix = "taxref_",
#'   csv_cols_select = c(
#'     "RANG", "HABITAT", "FR", "GF", "MAR", "GUA", "SM", "SB",
#'     "SPM", "MAY", "EPA", "REU", "SA", "TA", "TAAF", "PF", "NC", "WF", "CLI", "URL"
#'   )
#' )
#'
#' data_fungi_cleanNames_2 <- tax_info_pq(
#'   data_fungi_cleanNames,
#'   file_name = TAXREFv18_fungi,
#'   csv_taxonomic_rank = "NOM_VALIDE_SIMPLE",
#'   use_duck_db = TRUE,
#'   col_prefix = "taxref_",
#'   csv_cols_select = c("RANG", "HABITAT", "FR", "URL", "CD_REF")
#' )
#' table(data_fungi_cleanNames_2@tax_table[, "taxref_FR"])
#' table(data_fungi_cleanNames_2@tax_table[, "taxref_HABITAT"])
#'
#' # TAXREF example (with status)
#' # --------------------
#'
#' taxref_status <- system.file("extdata", "bdc_18_01_wider_mini.csv", package = "taxinfo")
#' data_fungi_cleanNames_3 <- tax_info_pq(data_fungi_cleanNames_2,
#'   taxonomic_rank = "taxref_CD_REF",
#'   file_name = taxref_status,
#'   csv_taxonomic_rank = "CD_REF",
#'   col_prefix = "st_",
#'   use_duck_db = TRUE
#' )
#'
#' data_fungi_cleanNames_3@tax_table[, "st_BCD_LRR"] |>
#'   table(useNA = "always")
#' data_fungi_cleanNames_3@tax_table[, "st_BCD_ZDET"] |>
#'   table(useNA = "always")
#' data_fungi_cleanNames_3@tax_table[, "st_BCD_TAXREF_STATUT_BIOGEO"] |>
#'   table(useNA = "always")
#'
#' #' # EPPO (Pest species) example (https://gd.eppo.int/)
#' # --------------------
#' # You can visit https://gd.eppo.int/ to download database for other countries
#' # than France
#' EPPO_FR <- system.file("extdata", "EPPO_regulated_FR.csv", package = "taxinfo")
#'
#' res_with_EPPO_FR <- tax_info_pq(data_fungi_cleanNames,
#'   file_name = EPPO_FR,
#'   csv_taxonomic_rank = "organism_prefname",
#'   col_prefix = "EPPO_"
#' )
#'
#' res_with_EPPO_FR@tax_table |>
#'   as.data.frame() |>
#'   filter(!is.na(EPPO_qlistlabel))
#' }
tax_info_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  file_name = NULL,
  csv_taxonomic_rank = NULL,
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  use_duck_db = FALSE,
  csv_cols_select = NULL,
  sep = ",",
  dec = ".",
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

  if (is.null(file_name)) {
    cli::cli_abort(
      "You must provide a file path to your CSV file with {.arg file_name}"
    )
  }

  if (!file.exists(file_name)) {
    cli::cli_abort("The file path {.path {file_name}} does not exist")
  }

  if (is.null(csv_taxonomic_rank)) {
    cli::cli_abort(
      "You must provide the name of the column containing taxonomic names with {.arg csv_taxonomic_rank}"
    )
  }

  csv_header <- read.csv(file_name, nrows = 1, sep = sep, dec = dec) |>
    colnames()
  if (!(csv_taxonomic_rank %in% csv_header)) {
    cli::cli_abort(
      "The parameter {.arg csv_taxonomic_rank} = {.val {csv_taxonomic_rank}} doesn't match any column in your CSV file"
    )
  }
  if (is.null(csv_cols_select)) {
    csv_cols_select <- csv_header
  }
  missing_cols <- setdiff(csv_cols_select, csv_header)
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} {.val {missing_cols}} of {.arg csv_cols_select} {?is/are} not in your CSV file."
    )
  }
  csv_cols <- unique(c(csv_taxonomic_rank, csv_cols_select))
  query_names <- unique(taxnames_vec)

  if (use_duck_db) {
    check_package("duckdb")
    check_package("dbplyr")
    con <- duckdb::dbConnect(duckdb::duckdb())
    on.exit(duckdb::dbDisconnect(con), add = TRUE)
    # Filter the csv in DuckDB to avoid reading the entire file into memory
    info_df <-
      tbl(con, file_name) |>
      select(all_of(csv_cols)) |>
      mutate(across(
        all_of(csv_taxonomic_rank),
        ~ sql(paste0(cur_column(), "::TEXT"))
      )) |>
      filter(!!sym(csv_taxonomic_rank) %in% query_names) |>
      collect()
  } else {
    info_df <- read.csv(
      file_name,
      sep = sep,
      dec = dec,
      colClasses = "character"
    ) |>
      select(all_of(csv_cols))
  }

  info_df <- as.data.frame(info_df)
  info_keys <- as.character(info_df[[csv_taxonomic_rank]])
  n_dup <- sum(duplicated(info_keys[info_keys %in% query_names]))
  if (verbose && n_dup > 0) {
    cli::cli_alert_info(
      "{.val {n_dup}} duplicated value{?s} of {.val {csv_taxonomic_rank}} in the CSV file: only the first row of each is used."
    )
  }
  info_tbl <- tibble(.csv_key = info_keys, info_df) |>
    filter(!duplicated(.data$.csv_key))

  if (verbose) {
    n_matched <- sum(query_names %in% info_tbl$.csv_key)
    cli::cli_alert_success(
      "Information from {.path {file_name}} ({.val {length(csv_cols)}} column{?s}) found for {.val {n_matched}} of the {.val {length(query_names)}} taxonomic names."
    )
  }

  if (!is.null(physeq)) {
    # The merge-back keeps the tax_table columns first (rank order) and an
    # existing `taxa_name` column untouched.
    new_physeq <- augment_tax_table(
      physeq,
      info_tbl,
      taxonomic_rank = taxonomic_rank,
      info_key = ".csv_key",
      col_prefix = col_prefix,
      keep_key = FALSE
    )
    if (add_to_phyloseq) {
      return(new_physeq)
    }
    return(tibble(as.data.frame(unclass(new_physeq@tax_table))))
  }

  tibble(taxa_name = taxnames_vec) |>
    left_join(info_tbl, by = c("taxa_name" = ".csv_key")) |>
    rename_with(~ paste0(col_prefix, .x), .cols = all_of(csv_cols))
}
