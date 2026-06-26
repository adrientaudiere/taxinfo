#' Get scientific works about taxa present in a phyloseq object
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#'   A wrapper of [openalexR::oa_fetch()] function to get the number of
#'   scientific works (and a list of doi if count_only is set to FALSE) for each
#'   taxa of a phyloseq object. Each taxa name is searched in the title and abstract
#'   of the works present in Open Alex database.
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames`
#'  must be provided, but not both.
#' @param taxnames (optional) A character vector of taxonomic names.
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param count_only (Logical, default FALSE) If
#'   TRUE, only the number of works on a given taxa is return, leading to a
#'   faster call to `openalexR::oa_fetch()`. Note that if count_only is set to TRUE
#'   all works (including e.g. preprint and dataset)
#'   are count, leading to higher number of works than if count_only is set to
#'   FALSE (see parameter `type_works`).
#' @param return_raw_oa (Logical, default FALSE) If TRUE, return the raw list of
#'   publications from Open Alex for each taxa as a list of data.frame. Can be
#'   useful to filter works for example by topic or by number of citations (see
#'   section examples). If TRUE, add_to_phyloseq is set to FALSE automatically.
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided,
#'   FALSE when taxnames is provided and FALSE if return_raw_oa is set to TRUE).
#'   If TRUE, return a new phyloseq object with new columns in the tax_table slot.
#'   Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#'   Cannot be TRUE if `taxnames` is provided.
#' @param col_prefix A character string to be added as a prefix to the new
#' columns names added to the tax_table slot of the phyloseq object (default: NULL).
#' @param type_works (A list of type to select)
#'  See Open Alex [documentation](https://docs.openalex.org/api-entities/works/work-object#type).
#'  Only used if count_only is set to FALSE Default is c("article", "review",
#'  "book-chapter", "book", "letter").
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param ... Other params to passed on [openalexR::oa_fetch()]
#' @param discard_genus_alone (logical, default `TRUE` when
#'  `taxonomic_rank == "currentCanonicalSimple"`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#' @param discard_NA (logical, default `TRUE`). Passed to
#'  [taxonomic_rank_to_taxnames()].
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with 1 (`n_doi`) or 4 (`n_doi`,
#'  `list_doi`, `n_citation` and `list_keywords`
#'  if `count_only` is FALSE) new column(s) in the tax_table.
#'
#'  - n_doi: number of publications citing this taxa in title or abstract
#'  - list_doi: list of DOIs separate by ";"
#'  - n_citation: total number of citations for all publications citing this taxa
#'  - list_keywords: list of keywords from all publications citing this taxa
#'
#' @export
#' @author Adrien Taudiere
#' @details
#' This function is mainly a wrapper of the work of others.
#'   Please cite `openalexR` package.
#' @examples
#' \dontrun{
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini) |>
#'   tax_oa_pq()
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
#'   col_prefix = "oa_",
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
#' }
tax_oa_pq <- function(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  count_only = FALSE,
  return_raw_oa = FALSE,
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  type_works = c("article", "review", "book-chapter", "book", "letter"),
  verbose = TRUE,
  discard_genus_alone = identical(taxonomic_rank, "currentCanonicalSimple"),
  discard_NA = TRUE,
  ...
) {
  check_package("openalexR")

  resolved <- resolve_taxa_input(
    physeq = physeq,
    taxnames = taxnames,
    add_to_phyloseq = add_to_phyloseq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = discard_genus_alone,
    discard_NA = discard_NA
  )
  taxnames <- resolved$taxnames
  add_to_phyloseq <- resolved$add_to_phyloseq

  if (return_raw_oa) {
    add_to_phyloseq <- FALSE
    cli::cli_alert_info(
      "{.arg add_to_phyloseq} is set to FALSE when {.arg return_raw_oa} is TRUE"
    )
  }

  if (length(taxnames) == 0) {
    cli::cli_warn(
      "No taxonomic names found for the specified taxonomic rank(s). Returning NULL."
    )
    return(NULL)
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
        cli::cli_alert_info(
          "Fetching OpenAlex works for taxon: {.emph {taxname}}"
        )
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
    tib_publi <- tibble(
      taxa_name = names(list_publi),
      n_doi = map_int(list_publi, ~ .x$count)
    )
  } else {
    list_publi <- vector("list", length(taxnames))
    list_publi <- map(
      taxnames,
      ~ {
        if (verbose) {
          cli::cli_alert_info("Fetching OpenAlex works for taxon: {.emph {.x}}")
        }
        possibly(openalexR::oa_fetch, otherwise = NULL)(
          entity = "works",
          title_and_abstract.search = .x
        )
      },
      .progress = ifelse(verbose, "Fetching OpenAlex", FALSE)
    )

    names(list_publi) <- taxnames
    list_publi[is.null(list_publi)] <- NA

    tib_publi <- list_publi |>
      map_dfr(~ .x |> as_tibble(), .id = "taxa_name") |>
      filter(type %in% type_works) |>
      mutate(
        keywords = map(
          keywords,
          ~ paste(as.vector(.x["display_name"][[1]]), collapse = ";")
        )
      )

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
    return(augment_tax_table(
      physeq,
      tib_publi,
      taxonomic_rank = taxonomic_rank,
      col_prefix = col_prefix,
      default_prefix = "oa_"
    ))
  } else {
    return(tib_publi)
  }
}
