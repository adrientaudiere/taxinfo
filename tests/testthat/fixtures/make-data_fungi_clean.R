# Build the small, gna-cleaned phyloseq fixture used across the test suite.
#
# Why a fixture? Almost every taxinfo test needs a phyloseq whose names have
# been resolved by `gna_verifier_pq()` (it adds the `currentCanonicalSimple`
# column the downstream `tax_*_pq()` functions key on). Calling
# `gna_verifier_pq()` live in every file meant 43 network round-trips, and
# several files ran the heavy per-taxon network loops over the full
# `data_fungi` (1420 taxa). Freezing one tiny cleaned object makes the suite
# fast, deterministic and largely offline.
#
# Regenerate (needs network, no credentials required) with:
#   source("tests/testthat/fixtures/make-data_fungi_clean.R")
# from the package root, then commit the updated .rds.

suppressPackageStartupMessages(library(phyloseq))
devtools::load_all(quiet = TRUE)

data(data_fungi, package = "MiscMetabar")

clean_full <- gna_verifier_pq(data_fungi, add_to_phyloseq = TRUE)

# Anchor taxa: their cleaned `currentCanonicalSimple` names are asserted on
# directly in the tests. Keep these stable.
anchors <- c(
  "ASV105", # Xylodon flaviporus
  "ASV749", # Sistotrema raduloides
  "ASV717", # Stypella subgelatinosa
  "ASV29" # Basidiodendron eyrei
)
stopifnot(all(anchors %in% phyloseq::taxa_names(clean_full)))

# A few extra species-level matches so network lookups return real data.
cc <- as.character(clean_full@tax_table[, "currentCanonicalSimple"])
extra_species <- c("Fomes fomentarius", "Exidia glandulosa", "Mycena renati")
extra <- phyloseq::taxa_names(clean_full)[cc %in% extra_species]

keep_taxa <- unique(c(anchors, head(extra, 3)))
fixture <- phyloseq::prune_taxa(keep_taxa, clean_full)

# Drop empty samples and keep a small, representative handful.
fixture <- phyloseq::prune_samples(
  phyloseq::sample_sums(fixture) > 0,
  fixture
)
keep_samples <- head(phyloseq::sample_names(fixture), 8)
fixture <- phyloseq::prune_samples(keep_samples, fixture)
fixture <- phyloseq::prune_taxa(phyloseq::taxa_sums(fixture) > 0, fixture)

message(
  "fixture: ", phyloseq::ntaxa(fixture), " taxa, ",
  phyloseq::nsamples(fixture), " samples"
)
message(
  "canonical names: ",
  paste(sort(unique(as.character(
    fixture@tax_table[, "currentCanonicalSimple"]
  ))), collapse = ", ")
)

saveRDS(
  fixture,
  file.path("tests", "testthat", "fixtures", "data_fungi_clean.rds")
)
