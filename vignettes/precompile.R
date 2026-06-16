#!/usr/bin/env Rscript

# Precompute the network-heavy package VIGNETTES.
#
# These vignettes enrich phyloseq objects via LIVE external APIs (GBIF, GNA,
# GLOBI, OpenAlex, Wikipedia). Rendering them live makes every R CMD build /
# check and every pkgdown build slow and flaky -- the same pattern already used
# for the `articles/external-information` article. To keep builds fast and
# reproducible we PRE-RENDER them:
#
#   * <vignette>.Rmd.orig  -> the editable SOURCE (edit this one)
#   * <vignette>.Rmd       -> GENERATED here (do NOT edit by hand)
#
# Unlike the article (which lives in the Rbuildignored `articles/` folder),
# these are REAL vignettes: the generated `.Rmd` AND the `figures/` they
# reference are built by R CMD build, so both must be committed.
#
# Workflow:
#
#   1. Only when you change a vignette's code/data (occasional; needs internet
#      and GBIF credentials -- GBIF_USER, GBIF_PWD, GBIF_EMAIL):
#        Rscript vignettes/precompile.R
#      then commit the regenerated files:
#        vignettes/getting-started.Rmd
#        vignettes/gbif-functions.Rmd
#        vignettes/checking-taxa-presence.Rmd
#        vignettes/figures/
#
#   2. Every time you rebuild docs (fast now -- no API calls here):
#        Rscript -e 'pkgdown::build_site()'

# Resolve this script's own directory so paths work from any CWD.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(file_arg) == 1) {
  normalizePath(dirname(file_arg))
} else {
  normalizePath("vignettes")
}

# Knit from vignettes/ so that `child = "includes/_styles.Rmd"` and
# `fig.path = "figures/"` (set in includes/_styles.Rmd) resolve to
# vignettes/figures/, exactly as they do during a normal vignette build.
old_wd <- setwd(script_dir)
on.exit(setwd(old_wd), add = TRUE)

vignettes <- c(
  "getting-started",
  "gbif-functions",
  "checking-taxa-presence"
)

for (v in vignettes) {
  src <- paste0(v, ".Rmd.orig")
  out <- paste0(v, ".Rmd")
  if (!file.exists(src)) {
    warning("Missing source: ", src, " -- skipping", call. = FALSE)
    next
  }
  message("Knitting ", src, " -> ", out)
  knitr::knit(input = src, output = out)
}

message(
  "Precompute done. Commit the regenerated *.Rmd vignettes and figures/."
)
