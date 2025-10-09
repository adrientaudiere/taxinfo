#' Extract spore size from mycoDB
#'
#' @description
#'  Extract spore size information from mycoDB (https://www.mycodb.fr/).
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'  The column(s) present in the @tax_table slot of the phyloseq object. Can
#'  be a vector of two columns (e.g. c("Genus", "Species")).
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param time_to_sleep (numeric, default 0.5) Time to sleep between two queries
#'  to mycoDB, in seconds.
#' @param add_to_phyloseq (logical, default NULL) If TRUE, add the spore size
#'  information to the phyloseq object. If FALSE, return a data.frame.
#'  If NULL (default), add to phyloseq if `physeq` is provided,
#'  else return a data.frame.
#'
#' @param col_prefix (character, default NULL)
#'  If not NULL, prefix to add to the new columns added to the phyloseq object.
#'
#' @returns If `add_to_phyloseq` is TRUE, returns a phyloseq object with
#' new columns in the tax_table slot: `spore_size`, `spore_length`, `spore_width`.
#' If `add_to_phyloseq` is FALSE, returns a data.frame with columns `taxa_name`,
#' `spore_size`, `spore_length`, `spore_width`.
#' @export
#' @author Adrien Taudière
#' 
#' @seealso [tax_info_pq()], [tax_gbif_occur_pq()]
#'
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <- data_fungi_mini |>
#'   gna_verifier_pq()
#' data_fungi_mini_spore_size <- tax_spores_size_pq(data_fungi_mini_cleanNames)
#'
#' psmelt(data_fungi_mini_spore_size) |>
#'   group_by(taxa_name) |>
#'   summarise(
#'     spore_length = as.numeric(unique(spore_length_mean)),
#'     spore_width = as.numeric(unique(spore_width_mean)),
#'     Abundance = sum(Abundance),
#'     Occurence = sum(Abundance > 0, na.rm = TRUE)
#'   ) |>
#'   ggplot(aes(x = spore_length, y = spore_width, size = Abundance, col = Occurence)) +
#'   geom_point(alpha = 0.7) +
#'   ggrepel::geom_text_repel(aes(label = taxa_name),
#'     vjust = -0.5,
#'     size = 3,
#'     fontface = "italic",
#'     min.segment.length = 0.2,
#'     force = 4
#'   ) +
#'   labs(
#'     title = "Spore sizes extracted from mycoDB",
#'     x = "Spore length (µm)",
#'     y = "Spore width (µm)",
#'     col = "Number of samples",
#'     size = "Number of sequences"
#'   ) +
#'   theme_idest()
#'
#' # Example with ellipses
#' psmelt(data_fungi_mini_spore_size) |>
#'   filter(!is.na(taxa_name) & !taxa_name == "") |>
#'   filter(!is.na(Time)) |>
#'   filter(Abundance > 0) |>
#'   mutate(taxa_name = as.factor(taxa_name)) |>
#'   group_by(taxa_name, Time) |>
#'   summarise(
#'     spore_length = 0.2 * as.numeric(unique(spore_length_mean)),
#'     spore_width = as.numeric(unique(spore_width_mean)),
#'     Abundance = sum(Abundance),
#'     Occurence = sum(Abundance > 0, na.rm = TRUE),
#'     Order = unique(Order)
#'   ) |>
#'   arrange(desc(Abundance)) |>
#'   mutate(
#'     taxa_name_num = as.numeric(taxa_name)
#'   ) |>
#'   filter(!is.na(spore_length)) |>
#'   ggplot(aes(x0 = log(Abundance), y0 = taxa_name_num / 5, a = spore_length / 2, b = spore_length / 2 / 5, fill = Order)) +
#'   coord_fixed() +
#'   ggforce::geom_ellipse(aes(angle = 0), alpha = 0.3) +
#'   ggrepel::geom_text_repel(aes(x = log(Abundance), y = taxa_name_num / 5, label = taxa_name, color = Order), size = 2) +
#'   theme_idest() +
#'   theme(axis.text.y = element_blank()) +
#'   labs(x = "Number of sequences (log scale)", y = "Taxa") +
#'   facet_wrap(~Time, ncol = 2)
#'
#'
#' # Test for difference in mean spore length between sample's factor
#' psmelt(data_fungi_mini_spore_size) |>
#'   filter(!is.na(taxa_name) & !taxa_name == "") |>
#'   filter(!is.na(spore_length_mean)) |>
#'   filter(!is.na(Time)) |>
#'   filter(Abundance > 0) |>
#'   mutate(taxa_name = as.factor(taxa_name)) |>
#'   group_by(taxa_name, Time) |>
#'   summarise(
#'     spore_length = unique(as.numeric(spore_length_mean)),
#'     spore_width = unique(as.numeric(spore_width_mean)),
#'     Order = unique(Order)
#'   ) |>
#'   ggstatsplot::ggbetweenstats(Time, spore_length)
#' }
tax_spores_size_pq <- function(physeq = NULL,
                               taxnames = NULL,
                               taxonomic_rank = "currentCanonicalSimple",
                               verbose = TRUE,
                               time_to_sleep = 0.5,
                               add_to_phyloseq = NULL,
                               col_prefix = NULL) {
  if (!is.null(taxnames) && !is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}, not both")
  }
  if (is.null(taxnames) && is.null(physeq)) {
    cli::cli_abort("You must specify either {.arg physeq} or {.arg taxnames}")
  }

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  }

  if (is.null(add_to_phyloseq)) {
    add_to_phyloseq <- !is.null(physeq)
  }

  spore_sizes <- sapply(taxnames, function(x) {
    Sys.sleep(time_to_sleep)
    extract_spores_mycodb(x, verbose = verbose)
  })

  spore_sizes_df <- data.frame(
    "spore_size" = spore_sizes,
    "min_left" = as.numeric(str_extract(spore_sizes, "^[0-9]+(\\.[0-9]+)?")),
    "max_left" = as.numeric(str_extract(spore_sizes, "(?<=-)[0-9]+(\\.[0-9]+)?(?=\\s*[×x])")),
    "min_right" = as.numeric(str_extract(spore_sizes, "(?<=×|x)\\s*[0-9]+(\\.[0-9]+)?")),
    "max_right" = as.numeric(str_extract(spore_sizes, "\\d+(?:\\.\\d+)?(?=\\s*µ?m?$)")),
    "taxa_name" = names(spore_sizes)
  ) |>
    mutate(
      # Determine which dimension should be length (larger mean)
      left_is_length = (max_left + min_left) >= (max_right + min_right),

      # Assign dimensions
      "spore_length_max" = ifelse(left_is_length,
        pmax(max_left, min_left),
        pmax(max_right, min_right)
      ),
      "spore_length_min" = ifelse(left_is_length,
        pmin(max_left, min_left),
        pmin(max_right, min_right)
      ),
      "spore_width_max" = ifelse(left_is_length,
        pmax(max_right, min_right),
        pmax(max_left, min_left)
      ),
      "spore_width_min" = ifelse(left_is_length,
        pmin(max_right, min_right),
        pmin(max_left, min_left)
      )
    ) |>
    mutate(
      "spore_length_mean" = (spore_length_max + spore_length_min) / 2,
      "spore_width_mean" = (spore_width_max + spore_width_min) / 2
    ) |>
    select(
      spore_size, spore_length_max,
      spore_length_min, spore_width_max,
      spore_width_min, spore_length_mean,
      spore_width_mean, taxa_name
    )

  if (add_to_phyloseq) {
    if (!is.null(col_prefix)) {
      spore_sizes_df <- spore_sizes_df |>
        rename_with(~ paste0(col_prefix, .))
    }

    new_physeq <- physeq
    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")
    new_physeq@tax_table <-
      left_join(tax_tab, spore_sizes_df, by = join_by(taxa_name)) |>
      as.matrix() |>
      tax_table()

    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    return(spore_sizes_df)
  }
}

#' Extract spore size from mycoDB for a single species
#'
#' @param species_name Character. Species name, e.g. "Amanita muscaria"
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#'
#' @returns A character string with the spore size, e.g. "8-10 x 6-8 µm".
#' If the species is not found in mycoDB, returns "Not in mycoDB", if
#' the species is found but no spore size info is available, returns
#' "No spore size info in mycoDB".
#'
#' @export
#' @author Adrien Taudière
#' @examples
#' extract_spores_mycodb("Amanita muscaria")
#' extract_spores_mycodb("Boletus edulis")
#' extract_spores_mycodb("Xylobolus subpileatus")
#' extract_spores_mycodb("Nonexistent species")
#' extract_spores_mycodb("Amanita")
extract_spores_mycodb <- function(species_name, verbose = TRUE) {
  base_url <- "https://www.mycodb.fr/fiche.php?"
  search_params <- paste0(
    "genre=", stringr::word(species_name, 1),
    "&espece=", stringr::word(species_name, 2)
  )
  page_html <- rvest::read_html(paste0(base_url, search_params))
  if (grepl(pattern = "https://www.mycodb.fr/quicksearch.php", rvest::html_text(page_html))
  ) {
    return("Not in mycoDB")
  } else {
    spore_size <- page_html |>
      rvest::html_element(
        xpath = "//*[contains(text(), 'Spores')]/following::p"
      ) |>
      rvest::html_text() |>
      stringr::str_extract_all("\\d+(?:[,.]\\d+)?(?:-\\d+(?:[,.]\\d+)?)?\\s*[×x]\\s*\\d+(?:[,.]\\d+)?(?:-\\d+(?:[,.]\\d+)?)?\\s*[μµ]?m?") |>
      unlist()
    if (length(spore_size) > 1) {
      if (verbose) {
        cli::cli_alert_warning("Multiple spore size entries found for {.emph {species_name}}. Using the first one.")
      }
      spore_size <- spore_size[1]
    }
    spore_size <- gsub(x = spore_size, ",", ".")
    if (is.na(spore_size) || length(spore_size) == 0) {
      spore_size <- "No spore size info in mycoDB"
    }
    return(spore_size)
  }
}
