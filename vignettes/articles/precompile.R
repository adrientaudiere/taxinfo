#!/usr/bin/env Rscript

# Precompute the network-heavy "external-information" article.
#
# This article enriches taxa via LIVE external APIs (Wikipedia, GLOBI,
# OpenAlex, GBIF, TAXREF). Rendering it live makes every pkgdown / R CMD build
# slow and flaky. To keep the built site fast and reproducible we PRE-RENDER it:
#
#   * external-information.Rmd.orig  -> the editable SOURCE (edit this one)
#   * external-information.Rmd       -> GENERATED here (do not edit by hand)
#
# This script knits the .orig once (needs internet; a few minutes with the
# taxa caps already in place) into the .Rmd with every output and figure baked
# in. The generated .Rmd has no executable chunks left, so any later render of
# it -- including a LOCAL `pkgdown::build_site()` -- is instant and offline.
#
# Workflow (this package is rendered locally; docs/ is committed and served by
# GitHub Pages):
#
#   1. Only when you change this article's code/data (occasional, needs net):
#        Rscript vignettes/articles/precompile.R
#      then commit the regenerated:
#        vignettes/articles/external-information.Rmd
#        vignettes/articles/figures/
#
#   2. Every time you rebuild the site (fast now -- no API calls here):
#        Rscript -e 'pkgdown::build_site()'
#        git add docs/ && git commit && git push

# Resolve this script's own directory so paths work from any CWD.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(file_arg) == 1) {
  normalizePath(dirname(file_arg))
} else {
  normalizePath("vignettes/articles")
}

old_wd <- setwd(script_dir)
on.exit(setwd(old_wd), add = TRUE)

# `fig.path = "figures/"` (set in ../includes/_styles.Rmd) resolves to
# vignettes/articles/figures/ because we knit from this directory; the
# `child = "../includes/_styles.Rmd"` reference resolves the same way.
knitr::knit(
  input = "external-information.Rmd.orig",
  output = "external-information.Rmd"
)

message(
  "Precompute done. Commit external-information.Rmd and the figures/ directory."
)
