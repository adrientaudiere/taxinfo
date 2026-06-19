test_that("fungal_traits_guilds errors without physeq", {
  expect_error(fungal_traits_guilds(NULL))
})

test_that("fungal_traits_guilds adds FungalTraits columns when names are clean", {
  skip_on_cran()
  res <- fungal_traits_guilds(
    load_clean_pq(),
    add_consensus = FALSE,
    fg_tax_levels = character(0),
    verbose = FALSE
  )

  expect_s4_class(res, "phyloseq")
  expect_true("ft_primary_lifestyle" %in% colnames(res@tax_table))
})

test_that("fungal_traits_guilds runs gna_verifier_pq when names are absent", {
  skip_on_cran()
  # Pass a small RAW (unverified) phyloseq to exercise the internal
  # gna_verifier_pq() path triggered when currentCanonicalSimple is absent.
  raw_pq <- prune_taxa(taxa_names(data_fungi_mini)[1:5], data_fungi_mini)
  expect_false("currentCanonicalSimple" %in% colnames(raw_pq@tax_table))

  vcr::use_cassette("fungal_traits_gna", {
    res <- fungal_traits_guilds(
      raw_pq,
      gna_data_sources = 210,
      add_consensus = FALSE,
      fg_tax_levels = character(0),
      verbose = FALSE
    )
  })

  expect_s4_class(res, "phyloseq")
  expect_true("currentCanonicalSimple" %in% colnames(res@tax_table))
  expect_true("ft_primary_lifestyle" %in% colnames(res@tax_table))
})

test_that("fungal_traits_guilds returns tibble when add_to_phyloseq = FALSE", {
  skip_on_cran()
  res <- fungal_traits_guilds(
    load_clean_pq(),
    add_consensus = FALSE,
    fg_tax_levels = character(0),
    add_to_phyloseq = FALSE,
    verbose = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_true("ft_primary_lifestyle" %in% colnames(res))
})

test_that("ft_to_trophic_mode maps correctly", {
  expect_equal(ft_to_trophic_mode("wood_saprotroph"), "Saprotroph")
  expect_equal(ft_to_trophic_mode("plant_pathogen"), "Pathotroph")
  expect_equal(ft_to_trophic_mode("ectomycorrhizal"), "Symbiotroph")
  expect_equal(ft_to_trophic_mode("unspecified"), NA_character_)
  expect_true(is.na(ft_to_trophic_mode(NA_character_)))
  expect_equal(ft_to_trophic_mode("unknown_category"), "Other")
})
