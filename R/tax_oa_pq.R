#' Get scientific works about taxa present in a phyloseq object
#'
#' @description
#'   A wrapper of [openalexR::oa_fetch()] function to get the number of
#'   scientific works (and a list of doi if list_doi is set to TRUE) for each
#'   taxa of a phyloseq object
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param count_only (Logical, default FALSE) If
#'   TRUE, only the number of works on a given taxa is return, leading to a
#'   faster call to openalexR::oa_fetch(). Note that if count_only is set to FALSE
#'   all works (including e.g. preprint and dataset) are count, leading to
#'   higher number of works than if list_doi is set to TRUE.
#' @param return_raw_oa (Logical, default FALSE) If TRUE, return the raw list of
#'   publications from Open Alex for each taxa as a list of data.frame. Can be
#'   useful to filter works for example by topic or by number of citations (see
#'   section examples).
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided)
#'   If TRUE, return a new phyloseq object with new columns in the tax_table slot.
#'   Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#'   Cannot be TRUE if `taxnames` is provided.
#' @param type_works (A list of type to select) See Open Alex [documentation](https://docs.openalex.org/api-entities/works/work-object#type).
#' Only used if count_only is set to FALSE Default is c("article", "review",
#'  "book-chapter", "book", "letter").
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param ... Other params to passed on [openalexR::oa_fetch()]
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with 1 (`n_doi`) or 4 (`n_doi`,
#'  `list_doi`, `n_citation` and `list_keywords`
#'  if `count_only` is FALSE) new column(s) in the tax_table.
#' @export
#' @author Adrien Taudière
#' @details
#' This function is mainly a wrapper of the work of others.
#'   Please cite `openalexR` package.
#' @examples
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini) |>
#'   tax_oa_pq(data_fungi_mini_cleanNames)
#'
#' ggplot(
#'   subset_taxa(data_fungi_mini_cleanNames, !is.na(n_doi))@tax_table,
#'   aes(
#'     x = log10(as.numeric(n_doi)),
#'     y = forcats::fct_reorder(currentCanonicalSimple, as.numeric(n_doi))
#'   )
#' ) +
#'   geom_point(aes(col = Order)) +
#'   xlab("Number of Scientific Papers (log10 scale)")
#'
#' tax_oa_pq(data_fungi_mini_cleanNames, type_works = "dataset")
#'
#'
#' list_pub_raw <- tax_oa_pq(data_fungi_mini_cleanNames,
#'   return_raw_oa = TRUE
#' )
#'
#' list_pub_Health_science <- lapply(list_pub_raw, function(xx) {
#'   if (length(xx) == 0) {
#'     return(NULL)
#'   } else {
#'     filter(xx, map_lgl(topics, function(tibble_item) {
#'       if (is.null(tibble_item) || nrow(tibble_item) == 0) {
#'         return(FALSE)
#'       } else {
#'         any(grepl("Health science",
#'           tibble_item$display_name[tibble_item$type == "domain"],
#'           ignore.case = TRUE
#'         ))
#'       }
#'     }))
#'   }
#' })
#'
#'
#' list_pub_Ecology <- lapply(list_pub_raw, function(xx) {
#'   if (length(xx) == 0) {
#'     return(NULL)
#'   } else {
#'     filter(xx, map_lgl(topics, function(tibble_item) {
#'       if (is.null(tibble_item) || nrow(tibble_item) == 0) {
#'         return(FALSE)
#'       } else {
#'         any(grepl("Ecology",
#'           tibble_item$display_name[tibble_item$type == "subfield"],
#'           ignore.case = TRUE
#'         ))
#'       }
#'     }))
#'   }
#' })
#'
#' list_pub_at_least_ten_citations <-
#'   lapply(list_pub_raw, function(xx) {
#'     if (length(xx) == 0) {
#'       return(NULL)
#'     } else {
#'       filter(xx, cited_by_count > 10)
#'     }
#'   })
tax_oa_pq <- function(physeq = NULL,
                      taxnames = NULL,
                      taxonomic_rank = "currentCanonicalSimple",
                      count_only = FALSE,
                      return_raw_oa = FALSE,
                      add_to_phyloseq = NULL,
                      type_works = c("article", "review", "book-chapter", "book", "letter"),
                      verbose = TRUE,
                      ...) {
  check_package("openalexR")

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

  if (sum(return_raw_oa, add_to_phyloseq) > 1) {
    stop("You can not set to TRUE more than one of the parameters return_raw_oa and add_to_phyloseq.")
  }

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  }

  if (return_raw_oa) {
    if (verbose) {
      pb <- cli::cli_progress_bar(total = length(taxnames))
    }

    list_publi <- vector("list", length(taxnames))
    for (i in seq_along(taxnames)) {
      taxname <- taxnames[i]
      if (verbose) {
        cli::cli_progress_update(id = pb, set = i)
        cli::cli_alert_info("Fetching OpenAlex works for taxon: {.emph {taxname}}")
      }
      list_publi[[i]] <- openalexR::oa_fetch(
        entity = "works",
        title_and_abstract.search = taxname,
        ...
      )
    }
    if (verbose) {
      cli::cli_progress_done(id = pb)
    }
    names(list_publi) <- taxnames
    return(list_publi)
  }

  if (count_only) {
    list_publi <- lapply(taxnames, function(taxname) {
      openalexR::oa_fetch(
        entity = "works",
        title_and_abstract.search = taxname,
        count_only = TRUE,
        ...
      )
    })

    names(list_publi) <- taxnames
    list_publi[is.null(list_publi)] <- NA
    tib_publi <- list_publi |>
      purrr::map_dfr(~ .x |> as_tibble(), .id = "taxa_name") |>
      select(taxa_name, count) |>
      rename(n_doi = count)
  } else {
    if (verbose) {
      pb <- cli::cli_progress_bar(total = length(taxnames))
    }

    list_publi <- vector("list", length(taxnames))
    for (i in seq_along(taxnames)) {
      taxname <- taxnames[i]
      if (verbose) {
        cli::cli_progress_update(id = pb, set = i)
        cli::cli_alert_info("Fetching OpenAlex works for taxon: {.emph {taxname}}")
      }
      list_publi[[i]] <- openalexR::oa_fetch(
        entity = "works",
        title_and_abstract.search = taxname,
        options = list(select = c("id", "doi", "type", "cited_by_count", "keywords")),
        ...
      )
    }
    if (verbose) {
      cli::cli_progress_done(id = pb)
    }

    names(list_publi) <- taxnames
    list_publi[is.null(list_publi)] <- NA

    check_package("purrr")
    tib_publi <- list_publi |>
      purrr::map_dfr(~ .x |> as_tibble(), .id = "taxa_name") |>
      filter(type %in% type_works) |>
      mutate(keywords = map(keywords, ~ paste(as.vector(.x["display_name"][[1]]), collapse = ";")))

    tib_publi <- tib_publi |>
      group_by(taxa_name) |>
      filter(!is.na(doi)) |>
      summarise(
        n_doi = n(),
        list_doi = paste0(doi, collapse = "; "),
        n_citation = sum(cited_by_count),
        list_keywords = paste0(keywords, collapse = ";")
      ) |>
      arrange(desc(n_doi))
  }

  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]),
      1, paste0,
      collapse = " "
    )

    new_physeq@tax_table <-
      full_join(tax_tab, tib_publi, by = join_by(taxa_name)) |>
      as.matrix() |>
      tax_table()

    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    return(tib_publi)
  }
}
