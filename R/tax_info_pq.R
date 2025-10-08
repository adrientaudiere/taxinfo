#' Get information from a custom csv file using taxonomic names present in a
#' phyloseq object
#'
#' @description
#'  A function to add information from a custom csv file (e.g. FungalTraits,
#'  Taxref, ...) to the tax_table slot of a phyloseq object by joining
#'  taxonomic names from phyloseq object (column `taxonomic_rank`) with a
#'  column of the csv file (`csv_taxonomic_rank`) containing the correspondant
#'  taxonomic names. Be carefull that the taxonomic names in the csv file must
#'  match exactly the taxonomic names in the phyloseq object. For example, if
#'  the taxonomic names in the phyloseq object are in the form "Genus species"
#'  the taxonomic names in the csv file must be in the same form
#'  (not "Genus_species" or "Genus Species Author"...).
#'
#'  Note that the csv file need to be in a wide-format, i.e. one line for each
#'  distinct value in the `csv_taxonomic_rank` columns. You may want to transform
#'  your data.frame using [tidyr::pivot_wider()] fonctions prior to write it in
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
#' columns names added to the tax_table slot of the phyloseq object.
#' @param use_duck_db (logical, default FALSE) If TRUE, use duckdb to handle
#'  the join between the csv file and the tax_table of the phyloseq object.
#'  Useful for large csv files.
#' @param csv_cols_select A character vector of the column names to select in the csv file.
#' @param sep the field separator character. See [utils::read.csv()].
#' @param dec the field separator character. See [utils::read.csv()].
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#' object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#' @author Adrien Taudière
#' @export
#'
#' @examples
#'
#' data_fungi_cleanNames <- gna_verifier_pq(data_fungi,
#'   data_sources = 210
#' )
#'
#' # FUNGAL TRAITS example
#' # --------------------
#' fungal_traits <- system.file("extdata", "fun_trait_mini.csv", package = "taxinfo")
#' fg_traits <- tax_info_pq(data_fungi_cleanNames,
#'   taxonomic_rank = "genus",
#'   file_name = fungal_traits,
#'   csv_taxonomic_rank = "GENUS",
#'   col_prefix = "ft_",
#'   sep = ";",
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
#'   csv_cols_select = c("RANG", "HABITAT", "FR", "GF", "MAR", "GUA", "SM", "SB", "SPM", "MAY", "EPA", "REU", "SA", "TA", "TAAF", "PF", "NC", "WF", "CLI", "URL")
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
tax_info_pq <- function(physeq = NULL,
                        taxnames = NULL,
                        taxonomic_rank = "currentCanonicalSimple",
                        file_name = NULL,
                        csv_taxonomic_rank = NULL,
                        add_to_phyloseq = NULL,
                        col_prefix = NULL,
                        use_duck_db = FALSE,
                        csv_cols_select = NULL,
                        sep = ",",
                        dec = ".") {
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}, not both")
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }

  # Set default for add_to_phyloseq based on input type
  if (is.null(add_to_phyloseq)) {
    add_to_phyloseq <- !is.null(physeq)
  }

  if (!is.null(taxnames) && add_to_phyloseq) {
    cli::cli_abort("{.arg add_to_phyloseq} cannot be TRUE when {.arg taxnames} is provided")
  }

  if (is.null(file_name)) {
    cli::cli_abort("You must provide a file path to your CSV file with {.arg file_name}")
  }

  if (!file.exists(file_name)) {
    cli::cli_abort("The file path {.path {file_name}} does not exist")
  }

  if (is.null(csv_taxonomic_rank)) {
    cli::cli_abort("You must provide the name of the column containing taxonomic names with {.arg csv_taxonomic_rank}")
  }

  if (is.null(taxnames)) {
    taxnames_vec <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  } else {
    taxnames_vec <- taxnames
  }

  if (!is.null(physeq)) {
    new_physeq <- physeq
    taxtab <- tibble(as.data.frame(new_physeq@tax_table))
    taxtab$taxa_name <- trimws(apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " "))
    taxtab$taxa_id_for_join <- 1:ntaxa(physeq)
  } else {
    taxtab <- tibble(taxa_name = taxnames_vec, taxa_id_for_join = 1:length(taxnames_vec))
  }

  if (is.null(csv_cols_select)) {
    # Read the first line of the csv file to get the column names
    csv_cols_select <- read.csv(file_name, nrows = 1, sep = sep, dec = dec) |>
      colnames()
  } else {
    csv_cols_select <- csv_cols_select
  }

  if (use_duck_db) {
    check_package("duckdb")
    con <- duckdb::dbConnect(duckdb::duckdb())
    # Summarize the dataset in DuckDB to avoid reading the entire CSV
    # into R's memory
    taxtab_new <-
      tbl(con, file_name) |>
      select(all_of(unique(c(csv_taxonomic_rank, csv_cols_select)))) |>
      mutate(across(all_of(csv_taxonomic_rank), ~ sql(paste0(cur_column(), "::TEXT")))) |>
      distinct(!!sym(csv_taxonomic_rank), .keep_all = TRUE) |>
      right_join(taxtab,
        by = join_by(!!sym(csv_taxonomic_rank) == taxa_name),
        copy = TRUE,
        suffix = c("", ".physeq")
      ) |>
      arrange(taxa_id_for_join) |>
      collect() |>
      select(-taxa_id_for_join) |>
      rename_with(.cols = all_of(c(csv_taxonomic_rank, csv_cols_select)), ~ paste0(col_prefix, .))

    duckdb::dbDisconnect(con)
  } else {
    info_df <- read.csv(file_name, sep = sep, dec = dec, colClasses = "character")

    if (!(csv_taxonomic_rank %in% colnames(info_df))) {
      cli::cli_abort("The parameter {.arg csv_taxonomic_rank} = {.val {csv_taxonomic_rank}} doesn't match any column in your CSV file")
    }

    taxtab_new <- info_df |>
      select(all_of(c(csv_taxonomic_rank, csv_cols_select))) |>
      distinct(!!sym(csv_taxonomic_rank), .keep_all = TRUE) |>
      right_join(taxtab,
        by = join_by(!!sym(csv_taxonomic_rank) == taxa_name)
      ) |>
      arrange(taxa_id_for_join) |>
      select(-taxa_id_for_join) |>
      rename_with(.cols = all_of(c(csv_taxonomic_rank, csv_cols_select)), ~ paste0(col_prefix, .))
  }
  if (add_to_phyloseq) {
    new_physeq@tax_table <-
      taxtab_new |>
      as.matrix() |>
      tax_table()
    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    return(taxtab_new)
  }
}
