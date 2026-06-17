# Test tax_globi_pq function
# Examples from man page: tax_globi_pq.Rd
#
# rglobi talks to GloBI over raw `curl`, which vcr cannot intercept. Instead we
# mock rglobi::get_interactions_by_taxa() with a tiny canned result and disable
# target-name verification (valid_taxo_target_taxon = FALSE) so the tests are
# fully offline and deterministic.

fake_globi_interactions <- function(sourcetaxon, ...) {
  data.frame(
    source_taxon_name = sourcetaxon,
    interaction_type = "hasHost",
    target_taxon_name = "Fagus sylvatica",
    stringsAsFactors = FALSE
  )
}

test_that("tax_globi_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_globi_pq(NULL))
})

test_that("tax_globi_pq returns tibble with add_to_phyloseq = FALSE", {
  skip_if_not_installed("rglobi")
  local_mocked_bindings(
    get_interactions_by_taxa = fake_globi_interactions,
    .package = "rglobi"
  )
  res_globi <- tax_globi_pq(
    load_clean_pq(),
    taxonomic_rank = c("Genus", "Species"),
    interaction_types = list("parasiteOf", "hasHost"),
    valid_taxo_target_taxon = FALSE,
    max_interactions = 10,
    add_to_phyloseq = FALSE
  )

  expect_s3_class(res_globi, "tbl_df")
  # Confirms the mock flowed through: the canned hasHost target is present.
  expect_true("hasHost" %in% names(res_globi))
  expect_true("Fagus sylvatica" %in% res_globi$hasHost)
})

test_that("tax_globi_pq returns phyloseq with add_to_phyloseq = TRUE", {
  skip_if_not_installed("rglobi")
  local_mocked_bindings(
    get_interactions_by_taxa = fake_globi_interactions,
    .package = "rglobi"
  )
  result <- tax_globi_pq(
    load_clean_pq(),
    interaction_types = c("hasHost"),
    valid_taxo_target_taxon = FALSE
  )

  expect_s4_class(result, "phyloseq")
})

test_that("tax_globi_pq add_to_phyloseq cannot be TRUE with taxnames", {
  expect_error(
    tax_globi_pq(taxnames = c("Amanita muscaria"), add_to_phyloseq = TRUE),
    "cannot be TRUE when.*taxnames"
  )
})

test_that("tax_globi_pq both physeq and taxnames causes error", {
  expect_error(
    tax_globi_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("tax_globi_pq verbose parameter works", {
  skip_if_not_installed("rglobi")
  local_mocked_bindings(
    get_interactions_by_taxa = fake_globi_interactions,
    .package = "rglobi"
  )
  result <- tax_globi_pq(
    load_clean_pq(),
    taxonomic_rank = c("Genus", "Species"),
    interaction_types = c("hasHost"),
    valid_taxo_target_taxon = FALSE,
    max_interactions = 10,
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_df")
})

test_that("tax_globi_pq discard_synonym parameter works", {
  skip_if_not_installed("rglobi")
  local_mocked_bindings(
    get_interactions_by_taxa = fake_globi_interactions,
    .package = "rglobi"
  )
  clean <- load_clean_pq()
  result_discard <- tax_globi_pq(
    clean,
    taxonomic_rank = c("Genus", "Species"),
    interaction_types = c("hasHost"),
    valid_taxo_target_taxon = FALSE,
    max_interactions = 10,
    discard_synonym = TRUE,
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result_discard, "tbl_df")

  result_keep <- tax_globi_pq(
    clean,
    taxonomic_rank = c("Genus", "Species"),
    interaction_types = c("hasHost"),
    valid_taxo_target_taxon = FALSE,
    max_interactions = 10,
    discard_synonym = FALSE,
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result_keep, "tbl_df")
})
