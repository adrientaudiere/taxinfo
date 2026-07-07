#' Add FAPROTAX functional-group annotations to a phyloseq object
#'
#' @description
#'
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Annotates the taxa of a phyloseq object with the ecological/metabolic
#' functional groups of the FAPROTAX database (Louca et al. 2016,
#' \doi{10.1126/science.aaf4507}). FAPROTAX maps cultured prokaryotic taxa
#' (mostly at the genus and species level) to functions such as
#' `methanogenesis`, `aerobic_ammonia_oxidation`, `sulfate_respiration`, etc.
#'
#' The bundled database file is parsed and every functional group is matched
#' against the taxonomic lineage of each taxon. A taxon is assigned to a group
#' when one of the group's `*level*level*` patterns is found, in order, along
#' its lineage (matching is case-insensitive, exactly as in FAPROTAX). The
#' `add_group:` / `remove_group:` / `intersect_group:` set operations used to
#' build composite groups are evaluated in file order.
#'
#' By default a single summary column (`faprotax_groups`, a `;`-separated list
#' of assigned groups) and a count column (`faprotax_n_groups`) are added to
#' the `tax_table`. Set `binary = TRUE` to additionally get one 0/1 column per
#' functional group that was assigned to at least one taxon.
#'
#' @param physeq (required) A phyloseq object.
#' @param faprotax_file (Character) Path to the FAPROTAX database text file.
#'   Defaults to the version bundled with the package
#'   (`system.file("extdata", "FAPROTAX.txt", package = "taxinfo")`).
#' @param tax_levels (Character vector) Names of the `tax_table` columns, from
#'   the highest to the lowest rank, that make up the lineage matched against
#'   FAPROTAX. Defaults to the 7 standard ranks. Missing columns are silently
#'   skipped.
#' @param col_prefix (Character, default `"faprotax_"`) Prefix applied to all
#'   columns added to the `tax_table`.
#' @param binary (Logical, default `FALSE`) If `TRUE`, add one integer 0/1
#'   column per functional group (prefixed with `col_prefix`) in addition to
#'   the summary columns.
#' @param valid_word_symbols (Character, default `"-"`) Non-alphanumeric
#'   characters that count as part of a word when matching pattern tokens
#'   against the lineage, matching the official FAPROTAX
#'   `--valid_word_symbols` option. Every other character (including `_`) is a
#'   word boundary, so e.g. `*Methanobacterium*` matches the GTDB name
#'   `Methanobacterium_B`.
#' @param add_to_phyloseq (Logical, default `TRUE`) If `TRUE`, return an updated
#'   phyloseq object. If `FALSE`, return a tibble of the augmented `tax_table`.
#' @param verbose (Logical, default `TRUE`) If `TRUE`, print progress messages.
#'
#' @returns Either an updated phyloseq object (when `add_to_phyloseq = TRUE`)
#'   or a tibble of the augmented `tax_table`.
#'
#' @details
#' FAPROTAX is a manually curated database built from cultured representatives,
#' with names following the NCBI/Bergey taxonomy. Coverage is therefore highest
#' for classically named taxa and can be very low for environmental lineages
#' known only from GTDB placeholder names (e.g. `JAJZYD01`). This is expected
#' behaviour, not a bug: unmatched taxa simply receive `NA`.
#'
#' The bundled `FAPROTAX.txt` is redistributed verbatim, including its original
#' copyright notice and BSD-style license (Copyright (c) 2019, Stilianos Louca).
#'
#' @references
#' Louca, S., Parfrey, L. W., & Doebeli, M. (2016). Decoupling function and
#' taxonomy in the global ocean microbiome. *Science*, 353(6305), 1272-1277.
#' \doi{10.1126/science.aaf4507}
#'
#' @author Adrien Taudiere
#' @export
#'
#' @seealso [add_metatraits_pq()], [fungal_traits_guilds()], [tax_info_pq()]
#'
#' @examples
#' data(GlobalPatterns, package = "phyloseq")
#'
#' res <- add_faprotax_pq(GlobalPatterns, verbose = FALSE)
#' 
#' head(sort(table(res@tax_table[, "faprotax_groups"], useNA = "ifany"), decreasing = TRUE))
#'
#' \donttest{
#' # One 0/1 column per functional group, then count the nitrifying bacteria
#' res_bin <- add_faprotax_pq(GlobalPatterns, binary = TRUE, verbose = FALSE)
#' sum(as.integer(res_bin@tax_table[, "faprotax_nitrification"]), na.rm = TRUE)
#'
#' # Restrict matching to the genus / species level only (fewer hits)
#' res_gs <- add_faprotax_pq(
#'   GlobalPatterns,
#'   tax_levels = c("Genus", "Species"),
#'   verbose = FALSE
#' )
#'
#' # Return a tibble instead of a phyloseq object
#' tib <- add_faprotax_pq(GlobalPatterns, add_to_phyloseq = FALSE, verbose = FALSE)
#' }
add_faprotax_pq <- function(
  physeq,
  faprotax_file = system.file(
    "extdata",
    "FAPROTAX.txt",
    package = "taxinfo"
  ),
  tax_levels = c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species"
  ),
  col_prefix = "faprotax_",
  binary = FALSE,
  valid_word_symbols = "-",
  add_to_phyloseq = TRUE,
  verbose = TRUE
) {
  if (is.null(physeq) || !methods::is(physeq, "phyloseq")) {
    cli::cli_abort("{.arg physeq} must be a {.cls phyloseq} object.")
  }
  if (!nzchar(faprotax_file) || !file.exists(faprotax_file)) {
    cli::cli_abort("FAPROTAX database file not found: {.path {faprotax_file}}.")
  }

  # Step 1: build the per-taxon lineage strings ------------------------------
  valid_levels <- tax_levels[tax_levels %in% colnames(physeq@tax_table)]
  if (length(valid_levels) == 0) {
    cli::cli_abort(
      c(
        "None of the {.arg tax_levels} are present in the {.field tax_table}.",
        "i" = "Available columns: {.val {colnames(physeq@tax_table)}}."
      )
    )
  }
  if (verbose && length(valid_levels) < length(tax_levels)) {
    cli::cli_alert_info(
      "Using ranks {.val {valid_levels}} (missing: {.val {setdiff(tax_levels, valid_levels)}})."
    )
  }

  tax_mat <- as.matrix(unclass(physeq@tax_table))[, valid_levels, drop = FALSE]
  lineage <- apply(tax_mat, 1, function(row) {
    row <- row[!is.na(row) & nzchar(trimws(row))]
    tolower(trimws(paste(row, collapse = " ")))
  })
  lineage <- as.vector(lineage)

  # Step 2: parse FAPROTAX and compute group membership ----------------------
  groups <- parse_faprotax(faprotax_file)
  if (verbose) {
    cli::cli_alert_info(
      "Parsed {.val {length(groups)}} FAPROTAX functional group{?s}."
    )
  }

  membership <- faprotax_membership(groups, lineage, valid_word_symbols)

  # Step 3: build the new columns --------------------------------------------
  group_names <- colnames(membership)
  # Always a list of length n_taxa (apply() would simplify to a matrix when
  # every taxon matches the same number of groups).
  assigned <- lapply(
    seq_len(nrow(membership)),
    function(i) group_names[membership[i, ]]
  )

  groups_col <- vapply(
    assigned,
    function(x) {
      if (length(x) == 0) {
        NA_character_
      } else {
        paste(x, collapse = ";")
      }
    },
    character(1)
  )
  n_col <- lengths(assigned)

  new_cols <- data.frame(
    groups = groups_col,
    n_groups = as.integer(n_col),
    stringsAsFactors = FALSE
  )
  colnames(new_cols) <- paste0(col_prefix, c("groups", "n_groups"))

  if (binary) {
    keep <- colSums(membership) > 0
    if (any(keep)) {
      bin_df <- as.data.frame(
        matrix(
          as.integer(membership[, keep, drop = FALSE]),
          nrow = nrow(membership),
          dimnames = list(NULL, paste0(col_prefix, group_names[keep]))
        )
      )
      new_cols <- cbind(new_cols, bin_df)
    }
  }

  # Suffix any column that already exists in the tax_table so that re-running
  # the function (or an earlier annotation) never produces duplicate names.
  colnames(new_cols) <- disambiguate_new_cols(
    colnames(physeq@tax_table),
    colnames(new_cols),
    verbose = verbose
  )
  rownames(new_cols) <- taxa_names(physeq)

  if (verbose) {
    n_assigned <- sum(n_col > 0)
    cli::cli_alert_success(
      "Assigned {.val {sum(colSums(membership) > 0)}} FAPROTAX group{?s} to {.val {n_assigned}}/{.val {ntaxa(physeq)}} taxa."
    )
  }

  new_tax_tab <- cbind(as.data.frame(physeq@tax_table), new_cols)
  new_physeq <- physeq
  new_physeq@tax_table <- tax_table(as.matrix(new_tax_tab))
  rownames(new_physeq@tax_table) <- taxa_names(physeq)

  if (add_to_phyloseq) {
    return(new_physeq)
  } else {
    return(tibble::as_tibble(as.data.frame(new_physeq@tax_table)))
  }
}

# Internal helpers -------------------------------------------------------------

#' Parse a FAPROTAX database file
#'
#' Reads a FAPROTAX text file into an ordered list of functional groups. Each
#' element is the group name and, in file order, its definition elements:
#' taxon patterns (`*A*B*`) and the set directives `add_group`,
#' `remove_group` and `intersect_group`. The order is preserved because
#' FAPROTAX evaluates the set operations sequentially and only references
#' groups defined earlier in the file.
#'
#' @param faprotax_file Path to the FAPROTAX text file.
#' @returns A named list; each element is a list with `patterns` (a list of
#'   character vectors of tokens) and `ops` (a list of `list(type, target)`),
#'   interleaved in `elements` to keep the original order.
#' @noRd
parse_faprotax <- function(faprotax_file) {
  raw <- readLines(faprotax_file, warn = FALSE)

  groups <- list()
  current <- NULL

  directive_re <- paste0(
    "^(add_group|subtract_group|remove_group|intersect_group|member_of):(.+)$"
  )

  for (ln in raw) {
    # Drop inline comments and surrounding whitespace.
    ln <- sub("#.*$", "", ln)
    ln <- trimws(ln)
    if (!nzchar(ln)) {
      next
    }

    if (grepl("^\"?\\*", ln)) {
      # Taxon member pattern, e.g. *Methanosarcina*barkeri*
      if (is.null(current)) {
        next
      }
      pat <- gsub("^\"|\"$", "", ln)
      tokens <- strsplit(pat, "\\*")[[1]]
      tokens <- trimws(tokens)
      tokens <- tokens[nzchar(tokens)]
      if (length(tokens) > 0) {
        groups[[current]]$elements <- c(
          groups[[current]]$elements,
          list(list(type = "pattern", tokens = tokens))
        )
      }
    } else if (grepl(directive_re, ln)) {
      # add_group / remove_group / intersect_group / member_of
      if (is.null(current)) {
        next
      }
      type <- sub(directive_re, "\\1", ln)
      target <- trimws(sub(directive_re, "\\2", ln))
      target <- gsub("^\"|\"$", "", target)
      groups[[current]]$elements <- c(
        groups[[current]]$elements,
        list(list(type = type, target = target))
      )
    } else if (grepl("^\\S+\\s+\\S*:", ln)) {
      # Group header: "<group_name>\t<attr:value; ...>"
      current <- strsplit(ln, "\\s+")[[1]][1]
      if (is.null(groups[[current]])) {
        groups[[current]] <- list(elements = list())
      }
    }
    # Any other line is ignored.
  }

  groups
}

#' Compute FAPROTAX group membership for a set of lineages
#'
#' @param groups Output of [parse_faprotax()].
#' @param lineage Character vector of lowercased, space-separated lineage
#'   strings (one per taxon).
#' @param valid_word_symbols Non-alphanumeric characters that count as part of a
#'   word (i.e. that do *not* act as a word boundary), matching FAPROTAX's
#'   `--valid_word_symbols` option (default `"-"`). Note that this means `_`
#'   *is* a boundary, so a pattern `*Methanobacterium*` matches the GTDB name
#'   `Methanobacterium_B`, exactly as the official FAPROTAX tool does.
#' @returns A logical matrix with one row per taxon and one column per group,
#'   in file order.
#' @noRd
faprotax_membership <- function(groups, lineage, valid_word_symbols = "-") {
  group_names <- names(groups)
  n_taxa <- length(lineage)

  # Word characters = alphanumeric + valid_word_symbols; every other character
  # is a word boundary. This reproduces FAPROTAX's find_matches_to_words_
  # expression() semantics (and differs from PCRE's \\b, which treats "_" as a
  # word character).
  sym <- strsplit(valid_word_symbols, "")[[1]]
  word_class <- paste0(
    "A-Za-z0-9",
    paste0("\\", sym, collapse = "")
  )

  # Work on unique non-empty lineages to keep pattern matching cheap.
  uniq <- unique(lineage[nzchar(lineage)])
  match_cache <- new.env(parent = emptyenv())

  match_pattern <- function(tokens) {
    key <- paste(tokens, collapse = "\r")
    if (exists(key, envir = match_cache, inherits = FALSE)) {
      hit_uniq <- get(key, envir = match_cache, inherits = FALSE)
    } else {
      # A pattern matches when every token is found (case-insensitive, whole
      # word) along the lineage, with strictly increasing positions (order).
      pos <- lapply(tokens, function(tk) {
        re <- paste0(
          "(?<![", word_class, "])\\Q", tk, "\\E(?![", word_class, "])"
        )
        m <- regexpr(re, uniq, perl = TRUE, ignore.case = TRUE)
        as.integer(m)
      })
      hit_uniq <- rep(TRUE, length(uniq))
      prev <- rep(-1L, length(uniq))
      for (i in seq_along(pos)) {
        found <- pos[[i]] > 0 & pos[[i]] > prev
        hit_uniq <- hit_uniq & found
        prev <- ifelse(found, pos[[i]], prev)
      }
      assign(key, hit_uniq, envir = match_cache)
    }
    # Map unique-lineage hits back to the full taxon vector.
    out <- logical(n_taxa)
    if (any(hit_uniq)) {
      matched_lineages <- uniq[hit_uniq]
      out <- lineage %in% matched_lineages
    }
    out
  }

  membership <- matrix(
    FALSE,
    nrow = n_taxa,
    ncol = length(group_names),
    dimnames = list(NULL, group_names)
  )

  for (g in group_names) {
    col <- logical(n_taxa)
    for (el in groups[[g]]$elements) {
      if (el$type == "pattern") {
        col <- col | match_pattern(el$tokens)
      } else if (el$type == "add_group") {
        if (el$target %in% colnames(membership)) {
          col <- col | membership[, el$target]
        }
      } else if (el$type %in% c("subtract_group", "remove_group")) {
        if (el$target %in% colnames(membership)) {
          col <- col & !membership[, el$target]
        }
      } else if (el$type == "intersect_group") {
        if (el$target %in% colnames(membership)) {
          col <- col & membership[, el$target]
        }
      }
      # member_of is metadata only and does not affect membership.
    }
    membership[, g] <- col
  }

  membership
}
