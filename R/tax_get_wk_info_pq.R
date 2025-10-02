#' Retrieve information about taxa from wikipedia
#'
#' @details
#' Taxa with only genus name are discarded.
#'
#' @param physeq (optional) A phyloseq object. Either `physeq` or `taxnames` must be provided, but not both.
#' @param taxonomic_rank  (Character, default = "currentCanonicalSimple")
#'  The column(s) present in the @tax_table slot of the phyloseq object. Can
#'  be a vector of two columns (e.g. the c("Genus", "Species")).
#' @param taxnames (optional) A character vector of taxonomic names. If provided, `physeq` is ignored.
#' @param add_to_phyloseq (logical, default TRUE when physeq is provided, FALSE when taxnames is provided) 
#' If TRUE, a new phyloseq object is returned with new columns in the tax_table.
#' Automatically set to TRUE when a phyloseq object is provided and FALSE when taxnames is provided.
#' Cannot be TRUE if `taxnames` is provided.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param languages_pages (Character vector or NULL, default NULL)
#'  If not NULL, only the languages present in this vector will be queried.
#'    The language codes are the two- or three-letter codes defined by ISO 639-1.
#'    For example, c("en", "fr", "de") will query only the English, French and
#'    German Wikipedia pages. If NULL (default), all languages will be queried.
#'    See https://en.wikipedia.org/wiki/List_of_Wikipedias for the
#'    list of language codes. Note that some taxa may not have pages in the
#'    specified languages. In this case, the function will return NA for these
#'    taxa.
#' @param time_to_sleep (numeric, default 0.3) Time to sleep between two calls to
#' wikipedia API.
#' @param summarize_function_length A function to summarize the page length
#' across languages. Default is "mean".
#' @param summarize_function_views A function to summarize the page views
#' across languages. Default is "sum".
#' @param n_days (numeric, default 30) Number of days to consider for the page views.
#'
#' @returns Either a tibble (if add_to_phyloseq = FALSE) or a new phyloseq
#' object, if add_to_phyloseq = TRUE, with new column(s) in the tax_table.
#' The tibble contains the following columns:
#'   - `lang`: Number of languages in which the taxon has a wikipedia page
#'   - `page_length`: Mean length of the wikipedia pages (in characters)
#'   - `page_views`: Total number of page views over the last 'n_days' days
#'   - `taxon_id`: Wikidata taxon identifier (e.g. "Q10723171" for Stereum ostrea)
#'   - `taxa_name`: Taxonomic name used to query wikipedia
#'
#' @export
#'
#' @examples
#'
#' wk_info <- tax_get_wk_info_pq(subset_taxa_pq(
#'   data_fungi_mini_cleanNames,
#'   taxa_sums(data_fungi_mini_cleanNames@otu_table) > 20000
#' ))
#'
#' data_fungi_mini_cleanNames_wk_info <-
#'   tax_get_wk_info_pq(data_fungi_mini_cleanNames,
#'     add_to_phyloseq = TRUE
#'   )
#'
#' subset_taxa(data_fungi_mini_cleanNames_wk_info, !is.na(page_views)) |>
#'   tax_table() |>
#'   as.data.frame() |>
#'   distinct(currentCanonicalSimple, .keep_all = TRUE) |>
#'   ggplot(
#'     aes(
#'       x = log10(as.numeric(page_views) + 1),
#'       y = forcats::fct_reorder(currentCanonicalSimple, as.numeric(page_views)),
#'       col = Order
#'     )
#'   ) +
#'   geom_segment(aes(
#'     x = 0, xend = log10(as.numeric(page_views) + 1),
#'     y = currentCanonicalSimple, yend = currentCanonicalSimple
#'   ), linewidth = 0.4) +
#'   geom_point(aes(size = as.numeric(page_length)), shape = 15) +
#'   geom_text(aes(label = lang), size = 2, color = "black") +
#'   xlab("Page views log-10 transformed. Number denoted the number of language in #' wikipedia.
#'       Shape size is proportional to mean page lenght in wikipedia.") +
#'   ylab("")
tax_get_wk_info_pq <- function(physeq = NULL,
                               taxonomic_rank = "currentCanonicalSimple",
                               taxnames = NULL,
                               add_to_phyloseq = NULL,
                               verbose = TRUE,
                               languages_pages = NULL,
                               time_to_sleep = 0.3,
                               summarize_function_length = "mean",
                               summarize_function_views = "sum",
                               n_days = 30) {
  check_package("wikitaxa")

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

  if (is.null(taxnames)) {
    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  }

  if (verbose) {
    cli::cli_alert_info("Getting taxonomic IDs from Wikidata...")
  }
  taxids <- sapply(taxnames, wikitaxa::wt_data_id)

  wk_lang <- lapply(taxids, tax_get_wk_lang, languages_pages = languages_pages)


  if (verbose) {
    pb <- cli::cli_progress_bar(total = length(taxnames))
  }

  wk_pages_info <- Map(function(x, names_x) {
    if (verbose) {
      # Update progress bar
      idx <- which(names(wk_lang) == names_x)
      cli::cli_progress_update(id = pb, set = idx)
      cli::cli_alert_info("Getting page views from Wikipedia for {.emph {names_x}}")
    }
    tax_get_wk_pages_info(
      tib_list = x,
      languages_pages = languages_pages,
      time_to_sleep = time_to_sleep,
      summarize_function_length = summarize_function_length,
      summarize_function_views = summarize_function_views,
      n_days = n_days
    )
  }, wk_lang, names(wk_lang))

  # Complete progress bar
  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  tib_info_wk <- tibble(
    "lang" = sapply(wk_lang, nrow),
    "page_length" = sapply(wk_pages_info, function(x) x$page_length),
    "page_views" = sapply(wk_pages_info, function(x) x$page_views),
    "taxon_id" = taxids,
    "taxa_name" = taxnames
  )

  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- apply(unclass(new_physeq@tax_table[, taxonomic_rank]), 1, paste0, collapse = " ")

    new_physeq@tax_table <-
      full_join(tax_tab, tib_info_wk, by = join_by(taxa_name)) |>
      as.matrix() |>
      tax_table()

    rownames(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    return(tib_info_wk)
  }
}

#' Retrieve the wikipedia pages for a given Wikidata taxon identifier
#'
#' @description
#' Filter only wikipedia page link to a language with a two- or three-letter code
#' defined by ISO 639-1 or ISO 639-3 (e.g. "en" for English, "fr" for French,
#' "de" for German). We also add a list of more-than-three-letter codes for
#' some languages: c("zh-yue", "nds-nl", "ru-sib", "bat-smg", "fiu-vro",
#'  "roa-rup", "map-bms", "cbk-zam", "roa-tara", "tokipona", "be-tarask",
#'   "zh-min-nan", "zh-classical"))
#' @param taxon_id (Character string, required) The Wikidata taxon identifier (e.g. "Q10723171" for Xylobolus subpileatus)
#' @param languages_pages (Character vector)
#' If not NULL, only the languages present in this vector will be queried.
#'
#' @returns A tibble with three columns: "title", "site" and "lang". NA values
#'  are returned if wikipedia api return a response different from 200 or
#'  if the taxon_id is set to NA or "". If no
#'  wikipedia page is found in the all languages, a tibble with 0
#'  is returned.
#' @export
#'
#' @examples
#' tax_get_wk_lang("Q10723171")
#' tax_get_wk_lang("Q10723171") |>
#'   nrow()
#'
#' tax_get_wk_lang("Q10723171")
tax_get_wk_lang <- function(taxon_id, languages_pages = NULL) {
  if (is.na(taxon_id) | taxon_id == "") {
    tib_links <- tibble("title" = taxon_id, "site" = NA, "lang" = NA)
    return(tib_links)
  }

  tryCatch(
    {
      url <- paste0(
        "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
        taxon_id, "&format=json&props=sitelinks"
      )

      response <- httr::GET(url)
      if (httr::status_code(response) == 200) {
        data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

        if (!is.null(data$entities[[taxon_id]]$sitelinks)) {
          sitelinks <- data$entities[[taxon_id]]$sitelinks
          wikipedia_links <- sitelinks[grepl("wiki$", names(sitelinks))]

          title <- unlist(lapply(wikipedia_links, function(x) x[["title"]]))

          tib_links <- tibble("title" = title, "site" = names(wikipedia_links))
          tib_links <- tib_links |>
            filter(grepl("wiki$", site)) |>
            mutate(lang = gsub("wiki$", "", site)) |>
            # filter out commonswiki, specieswiki, etc.
            filter(stringr::str_length(lang) %in% c(2, 3) |
              lang %in% c(
                "zh-yue", "nds-nl", "ru-sib", "bat-smg",
                "fiu-vro", "roa-rup", "map-bms", "cbk-zam",
                "roa-tara", "tokipona", "be-tarask",
                "zh-min-nan", "zh-classical"
              ))

          if (!is.null(languages_pages)) {
            tib_links <- tib_links |>
              filter(lang %in% languages_pages)
          }
          return(tib_links)
        } else {
          tib_links <- tibble("title" = taxon_id, "site" = 0, "lang" = 0)
          return(tib_links)
        }
      } else {
        tib_links <- tibble("title" = taxon_id, "site" = NA, "lang" = NA)
        return(tib_links)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Error for taxon {.val {taxon_id}}: {.emph {e$message}}")
      return(NA)
    }
  )
}

#' Retrieve information about wikipedia pages for a given taxon id
#'
#' @description
#' Input can be either a taxon_id (Wikidata taxon identifier) or a tibble as
#' returned by [tax_get_wk_lang()].
#'
#' @param taxon_id (Character string, required) The Wikidata taxon identifier (e.g. "Q10723171" for Xylobolus subpileatus)
#' @param tib_list A tibble as returned by [tax_get_wk_lang()] with columns
#'   "title", "site" and "lang".
#' @param languages_pages (Character vector)
#' If not NULL, only the languages present in this vector will be queried.
#'    The language codes are the two- or three-letter codes defined by ISO 639-1.
#'    For example, c("en", "fr", "de") will query only the English, French and
#'    German Wikipedia pages.
#' @param time_to_sleep (numeric, default 0.3) Time to sleep between two calls to
#' wikipedia API.
#' @param summarize_function_length A function to summarize the page length
#' across languages. Default is "mean". Other options can be "sum", "median",
#'  "max", "min", etc.
#' @param summarize_function_views A function to summarize the page views
#'  across languages. Default is "sum". Other options can be "mean", "median",
#'  "max", "min", etc.
#' @param n_days (numeric, default 30) Number of days to consider for the
#'  page views.
#' @param start_date The start date for the page views. If NULL (default),
#'   the start date is set to 'n_days' before the end date.
#' @param end_date The end date for the page views. If NULL (default),
#'  the end date is set to yesterday's date.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#'
#' @returns A list with two elements:
#'  - `page_length`: Mean length of the wikipedia pages (in characters)
#'  - `page_views`: Total number of page views over the last 'n_days' days
#'
#' @author Adrien Taudière
#' @export
#'
#' @examples
#' tax_get_wk_pages_info("Q10723171")
#' tax_get_wk_pages_info("Q10723171", languages_pages = c("fr", "en"))
#' tax_get_wk_pages_info("Q10723171", languages_pages = c("fr"))
#'
#' pages_Q10723171 <- tax_get_wk_lang("Q10723171")
#' tax_get_wk_pages_info(tib_list = pages_Q10723171)
#' tax_get_wk_pages_info(
#'   tib_list = pages_Q10723171,
#'   summarize_function_length = "sum"
#' )
#' tax_get_wk_pages_info(
#'   tib_list = pages_Q10723171,
#'   summarize_function_length = "sum",
#'   n_days = 365
#' )
#'
#' tax_get_wk_pages_info(
#'   tib_list = pages_Q10723171,
#'   start_date = "2023-01-01",
#'   end_date = "2023-12-31"
#' )
tax_get_wk_pages_info <- function(taxon_id = NULL,
                                  tib_list = NULL,
                                  languages_pages = NULL,
                                  time_to_sleep = 0.3,
                                  summarize_function_length = "mean",
                                  summarize_function_views = "sum",
                                  n_days = 30,
                                  start_date = NULL,
                                  end_date = NULL,
                                  verbose = FALSE) {
  if (is.null(tib_list) & !is.null(taxon_id)) {
    tib_list_pages <- tax_get_wk_lang(taxon_id, languages_pages = languages_pages)
  } else if (!is.null(tib_list) & is.null(taxon_id)) {
    tib_list_pages <- tib_list
  } else {
    stop("Please provide either taxon_id or tib_list, not both.")
  }

  if (!is.null(languages_pages)) {
    tib_list_pages <- tib_list_pages |>
      filter(lang %in% languages_pages)
    if (nrow(tib_list_pages) == 0) {
      if (verbose) {
        cli::cli_alert_warning("No pages found for taxon ID {.val {taxon_id}} in languages: {.val {paste(languages_pages, collapse = ', ')}}")
      }
      return(tibble("page_length" = 0, "page_views" = 0))
    }
  }

  tryCatch(
    {
      if (nrow(tib_list_pages) == 0 | is.na(tib_list_pages$site[1]) | tib_list_pages$site[1] == 0) {
        if (verbose) {
          cli::cli_alert_warning("No pages found for taxon ID {.val {taxon_id}}")
        }
        return(list("page_length" = 0, "page_views" = 0))
      }

      pages_len <- c()
      lang_views <- c()

      for (site_name in tib_list_pages$site) {
        lang_code <- gsub("wiki$", "", site_name)
        title <- tib_list_pages$title[tib_list_pages$site == site_name]

        if (verbose) {
          cli::cli_alert_info("Getting page length for {.emph {title}} ({.val {lang_code}})")
        }
        wiki_url <- paste0(
          "https://", lang_code,
          ".wikipedia.org/w/api.php?action=query&format=json&prop=revisions&rvprop=size&titles=",
          URLencode(title)
        )

        wiki_response <- httr::GET(wiki_url)
        if (httr::status_code(wiki_response) == 200) {
          wiki_data <- jsonlite::fromJSON(httr::content(wiki_response, "text", encoding = "UTF-8"))
          pages <- wiki_data$query$pages

          if (length(pages) > 0) {
            page_id <- names(pages)[1]
            if (!is.null(pages[[page_id]]$revisions)) {
              pages_len <- c(pages_len, pages[[page_id]]$revisions$size)
            }
          }
        }

        Sys.sleep(time_to_sleep)

        if (verbose) {
          cli::cli_alert_info("Getting page views for {.emph {title}} ({.val {lang_code}})")
        }

        if (is.character(start_date)) {
          start_date <- as.Date(start_date)
        }
        if (is.character(end_date)) {
          end_date <- as.Date(end_date)
        }
        if (is.null(end_date) && is.null(start_date)) {
          end_date <- Sys.Date() - 1
          start_date <- end_date - n_days + 1
        }
        start_str <- format(start_date, "%Y%m%d")
        end_str <- format(end_date, "%Y%m%d")

        stats_url <- paste0(
          "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
          lang_code, ".wikipedia/all-access/all-agents/",
          URLencode(title, reserved = TRUE), "/daily/", start_str, "/", end_str
        )

        stats_response <- httr::GET(stats_url)
        if (httr::status_code(stats_response) == 200) {
          stats_data <- jsonlite::fromJSON(httr::content(stats_response, "text", encoding = "UTF-8"))

          if (!is.null(stats_data$items)) {
            lang_views <- c(lang_views, sum(stats_data$items$views, na.rm = TRUE))
          }
        }
      }

      f <- match.fun(summarize_function_length)
      pages_len <- f(pages_len)

      f <- match.fun(summarize_function_views)
      lang_views <- f(lang_views)

      Sys.sleep(time_to_sleep)

      return(list("page_length" = pages_len, "page_views" = lang_views))
    },
    error = function(e) {
      cli::cli_alert_warning("Error for taxon {.val {taxon_id}}: {.emph {e$message}}")
      return(NA)
    }
  )
}
