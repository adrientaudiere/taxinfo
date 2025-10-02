#' Verify taxonomic assignment using BLAST against NCBI nucleotide database
#'
#' @description
#' The idea is to take the binomial taxonomic name assigned to each ASV/OTU at
#' the Genus_species level, search for sequences in NCBI nucleotide database
#' corresponding to this taxon name (with some additional filters including
#' the marker name), retrieve the sequences in fasta format, and then perform
#' a BLAST search of retrieved sequences against the ASV/OTU sequences.
#'
#' We can therefore test for each ASV/OTU if the best BLAST hit corresponds
#' to the same taxon name as the one assigned to the ASV/OTU. Moreover, we
#' can also detect some cases where a better taxonomic assignment can be
#' proposed based on the BLAST results limited to species name already present
#' in the phyloseq object.
#'
#' @param physeq A phyloseq object
#' @param taxonomic_rank (required, default = "currentCanonicalSimple")
#' The column(s) present in the @tax_table slot of the phyloseq object. Can
#' be a vector of two columns (e.g. c("Genus", "Species")).
#' @param marker (required) A character vector of marker names to be used
#' in the search term. For example, c("ITS", "internal transcribed spacer")
#' for fungal ITS sequences.
#' Note that the marker names should be present in the title of the sequences
#' in NCBI nucleotide database.
#' @param id_cut (default: 99) minimum % identity to consider a BLAST hit
#' as a good match. A 100 value means that only perfect matches are considered
#' as good matches.
#' @param retmax (default: 500) maximum number of sequences to retrieve
#' from NCBI nucleotide database for each taxon name.
#' @param add_to_phyloseq (logical, default TRUE) If TRUE, a new phyloseq object
#' is returned with new columns in the tax_table.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param start_date The start date for the search. If NULL (default),
#' the search is not limited by date. The date must be in the format
#' "YYYY-MM-DD".
#' @param end_date () The end date for the search. If NULL (default), the search
#' is not limited by date. If start_date is not NULL and end_date is NULL,
#' the end_date is set to today's date. The date must be in the format
#' "YYYY-MM-DD".
#' @param min_length (int) Minimum sequence length to consider in the search.
#' @param max_length (int) Maximum sequence length to consider in the search.
#' @param refseq_only (logical, default FALSE) If TRUE, only sequences from the
#'  RefSeq database are retrieved. RefSeq is a curated non-redundant database
#'  of sequences from NCBI. If FALSE, all sequences from NCBI nucleotide database
#'  are retrieved. Note that using refseq_only = TRUE is experimental and may
#'  lead to no sequence retrieved for some taxon names.
#' @param sup_params (char) Additional parameters to be added to the search term.
#'   By default set to ("NOT uncultured[Title] NOT clone[Title]") to exclude
#'   uncultured and clone sequences.
#'  @param ... Additional parameters to be passed to
#'    [MiscMetabar::blast_to_phyloseq()] including: `nproc`, `e_value_cut` and
#'     `args_blastn`
#'
#' @returns Either a list (if add_to_phyloseq = FALSE) or a new phyloseq
#' object, if add_to_phyloseq = TRUE, with new columns based on the
#'  `tib_retroblast` tibble describe below:
#'
#'  The list is composed of two elements:
#'   1. `tib_retroblast`: A tibble with one row for each taxa of the phyloseq object:
#'      - `blast_queried`: (logical) queried names for sequences
#'      - `blast_result`: (logical) Number of queried names with at least one
#'       blast result
#'      - `good_assign`: (logical) Number of good assignation (best blast
#'      hit with %identity >= id_cut corresponding to the same taxon name
#'      as the one assigned to the ASV/OTU)
#'      - `alt_assign`: Number of alternative assignation proposed
#'      (best blast hit with %identity >= id_cut and taxon name present in
#'       the phyloseq object)
#'      - `taxa_name`: Taxonomic name used to query NCBI nucleotide database
#'
#'   2. `entrez_search`: A list of the rentrez::entrez_search results
#'   for each taxon name
#' @export
#' @author Adrien Taudière
#' @examples
#' res_retro <- tax_retroblast_pq(data_fungi_mini_cleanNames,
#'   marker = c("ITS", "internal transcribed spacer"),
#'   retmax = 10, id_cut = 99
#' )
#'
#' res_retro$tib_retroblast |>
#'   summarise(
#'     prop_good_assign = sum(good_assign) / sum(blast_result),
#'     n_alt_assign = sum(!is.na(alt_assign))
#'   )
#'
#' table(res_retro$tib_retroblast$alt_assign)
#'
#' res_retro_100 <- tax_retroblast_pq(data_fungi_mini_cleanNames,
#'   marker = c("ITS", "internal transcribed spacer"),
#'   retmax = 100, id_cut = 100
#' )
#'
#' # nb of queried names for sequences (id=100%)
#' res_retro_100$tib_retroblast$blast_queried |> sum()
#' # nb of queried names with at least one blast result (id=100%)
#' res_retro_100$tib_retroblast$blast_result |> sum()
#' # nb of good assignation (id=100%)
#' res_retro_100$tib_retroblast$good_assign |> sum()
#' # nb of alternative assignation proposed (id=100%)
#' res_retro_100$tib_retroblast$alt_assign |>
#'   is.na() |>
#'   sapply(isFALSE) |>
#'   sum()
tax_retroblast_pq <- function(physeq,
                              taxonomic_rank = "currentCanonicalSimple",
                              marker = NULL,
                              id_cut = 99,
                              retmax = 500,
                              add_to_phyloseq = TRUE,
                              verbose = TRUE,
                              start_date = NULL,
                              end_date = NULL,
                              min_length = 300,
                              max_length = 4000,
                              refseq_only = FALSE,
                              sup_params = "NOT uncultured[Title] NOT clone[Title]",
                              ...) {
  check_package("rentrez")

  taxnames <- taxonomic_rank_to_taxnames(
    physeq = physeq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = TRUE,
    discard_NA = TRUE
  )

  res_tax <- vector("list", length = length(taxnames))
  names(res_tax) <- taxnames
  res_tax2 <- vector("list", length = length(taxnames))
  names(res_tax2) <- taxnames
  search_res <- vector("list", length = length(taxnames))
  names(search_res) <- taxnames


  if (verbose) {
    pb <- cli::cli_progress_bar(total = length(taxnames))
  }

  for (i in seq_along(taxnames)) {
    tax_i <- taxnames[i]
    if (verbose) {
      cli::cli_progress_update(id = pb, set = i)
      cli::cli_alert_info("Processing taxon: {.emph {tax_i}}")
    }

    taxa_pq_i <- select_taxa_pq(physeq, taxnames = tax_i, taxonomic_rank = taxonomic_rank, verbose = FALSE) |>
      clean_pq(silent = TRUE) |>
      taxa_names()


    if (!is.null(start_date)) {
      if (is.null(end_date)) {
        end_date <- format(Sys.Date(), "%Y-%m-%d")
      }
      search_term <-
        paste0(
          tax_i, "[Organism]",
          # " AND ", marker, "[Title] ",
          paste0(" AND (", paste(paste0(marker, "[Title]"), collapse = " OR "), ")"),
          start_date, ":", end_date, "[PDAT]",
          " AND ", min_length, ":", max_length, "[SLEN] ",
          sup_params
        )
    } else {
      search_term <-
        paste0(
          tax_i, "[Organism]",
          ifelse(length(marker) == 1, paste0(" AND ", marker, "[Title]"),
            paste0(" AND (", paste(paste0(marker, "[Title]"),
              collapse = " OR "
            ), ")")
          ),
          " AND ", min_length, ":", max_length, "[SLEN] ",
          sup_params
        )
    }

    if (refseq_only) {
      search_term <- paste0(search_term, " AND refseq[Filter]")
    }
    search_res[[tax_i]] <- rentrez::entrez_search(
      db = "nucleotide",
      term = search_term,
      retmax = retmax
    )

    if (verbose) {
      cli::cli_alert_info("Search term: {.code {search_term}}")
      cli::cli_alert_info("Number of results for {.emph {tax_i}}: {.val {search_res[[tax_i]]$count}}")
      cli::cli_alert_info("Number of FASTA sequences retrieved: {.val {length(search_res[[tax_i]]$ids)}}")
    }
    if (search_res[[tax_i]]$count == 0) {
      if (verbose) {
        cli::cli_alert_warning("No sequence found for {.emph {tax_i}}")
      }
      res_tax_i <- FALSE
      res_tax[[tax_i]] <- res_tax_i
      next
    }
    if (length(search_res[[tax_i]]$ids) < 101) {
      search_id <- search_res[[tax_i]]$ids
      seq_fasta <- rentrez::entrez_fetch(
        db = "nucleotide",
        id = search_id,
        rettype = "fasta"
      )
    } else {
      search_id <-
        split(search_res[[tax_i]]$ids, ceiling(seq_along(search_res[[tax_i]]$ids) / 100))

      seq_fasta <- lapply(search_id, function(x) {
        rentrez::entrez_fetch(
          db = "nucleotide",
          id = x,
          rettype = "fasta"
        )
      })
      seq_fasta <- paste0(seq_fasta, collapse = "\n")
    }
    temp_fasta <- tempfile()
    write(seq_fasta, file = temp_fasta)

    res_b_raw <- blast_to_phyloseq(physeq,
      seq2search = temp_fasta,
      unique_per_seq = FALSE,
      id_cut = id_cut,
      ...
    )
    if (is.null(res_b_raw)) {
      res_tax_i <- FALSE
      res_tax[[tax_i]] <- res_tax_i
    } else {
      res_b <- res_b_raw |>
        group_by(`Taxa name`) |>
        arrange(desc(`% id. match`)) |>
        filter(row_number() == 1)

      taxa_blast <- res_b |>
        pull(`Taxa name`)

      res_tax_i <- taxa_pq_i == taxa_blast
      names(res_tax_i) <- taxa_pq_i
      res_tax[[tax_i]] <- res_tax_i

      res_tax2[[tax_i]] <- taxa_blast[!taxa_blast %in% taxa_pq_i]
    }
  }

  # Complete progress bar
  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  no_blast_results <- lapply(res_tax2, is.null) |>
    unlist()

  good_assignation <- tibble(
    "taxnames_species" = rep(names(res_tax), lengths(res_tax)),
    "good_assign" = unlist(res_tax, use.names = FALSE),
    "taxa_name" = sub(".*\\.", "", names(unlist(res_tax)))
  ) |>
    filter(!is.na(taxa_name)) |>
    filter(!taxa_name == "NA")

  alternative_assignation <- tibble(
    "alt_assign" = rep(names(res_tax2), lengths(res_tax2)),
    "taxa_name" = unlist(res_tax2, use.names = FALSE)
  )

  tib_retroblast <-
    tibble(
      "taxa_name" = taxa_names(physeq),
      "taxnames_species" =
        taxonomic_rank_to_taxnames(physeq,
          taxonomic_rank = taxonomic_rank,
          discard_genus_alone = FALSE,
          discard_NA = FALSE,
          distinct_names = FALSE
        ),
    ) |>
    mutate("blast_queried" = taxnames_species %in% taxnames) |>
    mutate("blast_result" = taxnames_species %in%
      names(no_blast_results)[!no_blast_results]) |>
    mutate("good_assign" = taxa_name %in% good_assignation$taxa_name[good_assignation$good_assign])

  if (nrow(alternative_assignation) > 0) {
    tib_retroblast <- tib_retroblast |>
      left_join(alternative_assignation, by = join_by(taxa_name)) |>
      mutate(good_assign = ifelse(is.na(alt_assign), good_assign, FALSE))
  } else {
    tib_retroblast <- tib_retroblast |>
      mutate("alt_assign" = FALSE)
  }
  if (add_to_phyloseq) {
    new_physeq <- physeq

    tax_tab <- as.data.frame(new_physeq@tax_table)
    tax_tab$taxa_name <- taxa_names(physeq)

    new_physeq@tax_table <-
      left_join(tax_tab, tib_retroblast, by = join_by(taxa_name)) |>
      as.matrix() |>
      tax_table()

    taxa_names(new_physeq@tax_table) <- taxa_names(physeq)

    return(new_physeq)
  } else {
    return(list(
      "tib_retroblast" = tib_retroblast,
      "entrez_search" = search_res
    ))
  }
}
