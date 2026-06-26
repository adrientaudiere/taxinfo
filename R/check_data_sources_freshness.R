#' Warn when a selected GNA data source is stale
#'
#' @description
#' <a href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle">
#' <img src="https://img.shields.io/badge/lifecycle-experimental-orange" alt="lifecycle-experimental"></a>
#'
#' Internal, best-effort helper used by [gna_verifier_pq()]. It looks up the last
#' update date of the requested Global Names Architecture (GNA) `data_sources`
#' (via [taxize::gna_data_sources()], cached once per session) and emits an
#' informative message for any source older than `max_age_days`. The data
#' sources are not all refreshed on the same schedule (see
#' <https://verifier.globalnames.org/data_sources>), so a stale source can
#' silently miss recently described or recombined taxa.
#'
#' The metadata lookup is itself a network call; it is wrapped in `tryCatch()`
#' and stays completely silent on any failure (offline, API change, ...), so it
#' never blocks name verification.
#'
#' @param data_sources (integer/character) The GNA data source id(s) passed to
#'  [gna_verifier_pq()].
#' @param max_age_days (numeric, default `365`) Age threshold, in days, above
#'  which a data source is reported as stale.
#'
#' @returns Invisibly `NULL`, called for its message side-effect.
#'
#' @author Adrien Taudiere
#' @keywords internal
#' @seealso [gna_verifier_pq()], [taxize::gna_data_sources()]
check_data_sources_freshness <- function(data_sources, max_age_days = 365) {
  tryCatch(
    {
      cache <- get(".taxinfo_cache", envir = asNamespace("taxinfo"))
      if (exists("gna_data_sources", envir = cache, inherits = FALSE)) {
        ds <- get("gna_data_sources", envir = cache, inherits = FALSE)
      } else {
        ds <- taxize::gna_data_sources()
        assign("gna_data_sources", ds, envir = cache)
      }

      sel <- ds[ds$id %in% data_sources, , drop = FALSE]
      if (nrow(sel) == 0) {
        return(invisible(NULL))
      }

      updated <- as.Date(substr(sel$updatedAt, 1, 10))
      is_stale <- !is.na(updated) & updated < (Sys.Date() - max_age_days)
      if (!any(is_stale)) {
        return(invisible(NULL))
      }

      stale_msgs <- paste0(
        sel$title[is_stale],
        " (id ",
        sel$id[is_stale],
        "): last updated ",
        updated[is_stale]
      )
      names(stale_msgs) <- rep(" ", length(stale_msgs))
      cli::cli_inform(c(
        "i" = "Some GNA {.arg data_sources} are older than {.val {max_age_days}} days; name resolution may miss recent taxa:",
        stale_msgs,
        "i" = "Compare update dates at {.url https://verifier.globalnames.org/data_sources}."
      ))
    },
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}
