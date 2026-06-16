# Check package availability and propose installation instructions

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-maturing-blue"
alt="lifecycle-maturing"\>\</a\>

This function checks if a package is available using requireNamespace.
If the package is not available, it provides helpful installation
instructions.

## Usage

``` r
check_package(
  package,
  repo = "CRAN",
  github_repo = NULL,
  stop_on_error = TRUE,
  quietly = TRUE
)
```

## Arguments

- package:

  (required) Character string. Name of the package to check.

- repo:

  Character string. Repository source for installation suggestion.
  Options: "CRAN" (default), "Bioconductor", "GitHub".

- github_repo:

  Character string. GitHub repository in format "username/repository".
  It overrides repo if provided. Required if repo is "GitHub".

- stop_on_error:

  Logical. If TRUE (default), stops execution when package is missing.
  If FALSE, returns FALSE and shows message.

- quietly:

  Logical. If TRUE, suppresses the requireNamespace loading messages.
  Default is TRUE.

## Value

Logical. TRUE if package is available, FALSE if not available.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check CRAN package
check_package("dplyr")

# Check Bioconductor package
check_package("Biostrings", repo = "Bioconductor")

# Check GitHub package
check_package("MiscMetabar",
  repo = "GitHub",
  github_repo = "adrientaudiere/MiscMetabar"
)

# Stop execution if package is missing
check_package("ggplot2", stop_on_error = TRUE)
} # }
```
