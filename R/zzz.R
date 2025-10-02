.onAttach <- function(libname, pkgname) {
  # Set custom CLI theme for the package
  cli::cli_div(theme = list(
    span.emph = list(color = "#e5d7b1"),
    span.val = list(color = "#2e7891"),
    span.arg = list(color = "#c8a734"),
    span.code = list(color = "#aa4c26")
  ), .envir = parent.frame())
}
