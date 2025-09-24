#' Get scientific works about taxa present in a phyloseq object
#'
#' @description
#'   A wrapper of [openalexR::oa_fetch()] function to get the number of
#'   scientific works (and a list of doi if list_doi is set to TRUE) for each
#'   taxa of a phyloseq object
#'
#' @param physeq A phyloseq object
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param list_doi (Logical, default TRUE) Do the list of doi is returned ? If
#'   false, only the number of works on a given taxa is return, leading to a
#'   faster call to openalexR::oa_fetch(). Note that if list_doi is set to FALSE
#'   all works (including e.g. preprint and dataset) are count, leading to
#'   higher number of works than if list_doi is set to TRUE.
#' @param return_raw_oa (Logical, default FALSE) If TRUE, return the raw list of
#'   publications from Open Alex for each taxa as a list of data.frame. Can be
#'   useful to filter works for example by topic or by number of citations (see
#'   section examples).
#' @param add_to_phyloseq If TRUE, return a new phyloseq
#'   object with new columns in the tax_table slot.
#' @param type_works (A list of type to select) See Open Alex [documentation](https://docs.openalex.org/api-entities/works/work-object#type).
#' Only used if list_doi is set to TRUE. Default is c("article", "review",
#'  "book-chapter", "book", "letter").
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param ... Other params to passed on [openalexR::oa_fetch()]
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#'  object, if add_to_phyloseq = TRUE, with 1 (n_doi) or 2 (n_doi and list_doi
#'  if `list_doi` is TRUE) new column(s) in the tax_table.
#' @export
#' @author Adrien Taudière
#' @details
#' This function is mainly a wrapper of the work of others.
#'   Please cite `openalexR` package.
#' @examples
#' data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini,
#'   add_to_phyloseq = TRUE
#' )
#' data_fungi_mini_cleanNames <- tax_oa_pq(data_fungi_mini_cleanNames,
#'   add_to_phyloseq = TRUE
#' )
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
tax_oa_pq <- function(physeq,
                      taxonomic_rank = "currentCanonicalSimple",
                      list_doi = TRUE,
                      return_raw_oa = FALSE,
                      add_to_phyloseq = FALSE,
                      type_works = c("article", "review", "book-chapter", "book", "letter"), verbose = TRUE,
                      ...) {
  check_package("openalexR")

  if (sum(list_doi, return_raw_oa, add_to_phyloseq) > 1) {
    stop("You can not set to TRUE more than one of the parameters list_doi, return_raw_oa and add_to_phyloseq.")
  }

  taxnames <- taxonomic_rank_to_taxnames(
    physeq = physeq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = TRUE
  )

  if (return_raw_oa) {
    list_publi <- lapply(taxnames, function(taxname) {
      if (verbose) {
        message("Fetching works for taxon: ", taxname)
      }
      openalexR::oa_fetch(
        entity = "works",
        title_and_abstract.search = taxname,
        ...
      )
    })
    names(list_publi) <- taxnames
    return(list_publi)
  }

  if (list_doi) {
    list_publi <- lapply(taxnames, function(taxname) {
      if (verbose) {
        message("Fetching works for taxon: ", taxname)
      }
      openalexR::oa_fetch(
        entity = "works",
        title_and_abstract.search = taxname,
        options = list(select = c("id", "doi", "type")),
        ...
      )
    })

    names(list_publi) <- taxnames
    list_publi[is.null(list_publi)] <- NA

    check_package("purrr")
    tib_publi <- list_publi |>
      purrr::map_dfr(~ .x |> as_tibble(), .id = "taxa_name") |>
      filter(type %in% type_works)

    tib_publi <- tib_publi |>
      group_by(taxa_name) |>
      filter(!is.na(doi)) |>
      summarise(n_doi = n(), list_doi = paste0(doi, collapse = "; ")) |>
      arrange(desc(n_doi))
  } else {
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
      select(taxa_name, count)
  }

  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")

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
