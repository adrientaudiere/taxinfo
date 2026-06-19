# Test tax_photos_pq function

# Minimal 2-taxa phyloseq for fast tests
make_mini_physeq <- function() {
  otu_mat <- matrix(
    c(10, 5, 8, 3),
    nrow = 2,
    dimnames = list(c("OTU1", "OTU2"), c("S1", "S2"))
  )
  tax_mat <- matrix(
    c(
      "Fungi",
      "Basidiomycota",
      "Xylodon",
      "Xylodon flaviporus",
      "Fungi",
      "Basidiomycota",
      "Basidiodendron",
      "Basidiodendron eyrei"
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("OTU1", "OTU2"),
      c("Kingdom", "Phylum", "Genus", "currentCanonicalSimple")
    )
  )
  samp <- data.frame(row.names = c("S1", "S2"), Loc = c("A", "B"))
  phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax_mat),
    phyloseq::sample_data(samp)
  )
}

test_that("tax_photos_pq input validation", {
  expect_error(tax_photos_pq(NULL))
  expect_error(
    tax_photos_pq(
      taxnames = c("Xylodon flaviporus"),
      add_to_phyloseq = TRUE
    )
  )
})

# taxnames + gallery=TRUE (gbif and wikitaxa)
test_that("tax_photos_pq taxnames + gallery=TRUE + gbif returns shiny.tag", {
  skip_on_cran()
  vcr::use_cassette("photos_gallery_gbif", {
    result <- tax_photos_pq(
      taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
      gallery = TRUE,
      source = "gbif",
      verbose = FALSE
    )
  })
  expect_s3_class(result, "shiny.tag")
})

test_that("tax_photos_pq taxnames + gallery=TRUE + wikitaxa returns shiny.tag", {
  skip_on_cran()
  vcr::use_cassette("photos_gallery_wikitaxa", {
    result <- tax_photos_pq(
      taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
      gallery = TRUE,
      source = "wikitaxa",
      verbose = FALSE
    )
  })
  expect_s3_class(result, "shiny.tag")
})

# physeq + add_to_phyloseq=TRUE + gallery=FALSE
test_that("tax_photos_pq physeq + add_to_phyloseq=TRUE + gallery=FALSE returns phyloseq", {
  skip_on_cran()
  physeq2 <- make_mini_physeq()
  vcr::use_cassette("photos_add_phyloseq", {
    for (src in c("gbif", "wikitaxa")) {
      result <- tax_photos_pq(
        physeq2,
        gallery = FALSE,
        add_to_phyloseq = TRUE,
        source = src,
        verbose = FALSE
      )
      expect_s4_class(result, "phyloseq")
      expect_true("photo_url" %in% colnames(result@tax_table))
    }
  })
})

# physeq + add_to_phyloseq=TRUE + gallery=TRUE + simple_caption=FALSE
# (was broken: taxa_match unnamed vector caused subset_taxa_pq to error)
test_that("tax_photos_pq physeq + gallery=TRUE + simple_caption=FALSE works", {
  skip_on_cran()
  physeq2 <- make_mini_physeq()
  vcr::use_cassette("photos_caption_false", {
    for (src in c("gbif", "wikitaxa")) {
      result <- tax_photos_pq(
        physeq2,
        gallery = TRUE,
        add_to_phyloseq = TRUE,
        simple_caption = FALSE,
        source = src,
        verbose = FALSE
      )
      expect_s4_class(result, "phyloseq")
    }
  })
})

# physeq + add_to_phyloseq=TRUE + gallery=TRUE + simple_caption=TRUE
test_that("tax_photos_pq physeq + gallery=TRUE + simple_caption=TRUE returns phyloseq", {
  skip_on_cran()
  physeq2 <- make_mini_physeq()
  vcr::use_cassette("photos_caption_true", {
    for (src in c("gbif", "wikitaxa")) {
      result <- tax_photos_pq(
        physeq2,
        gallery = TRUE,
        add_to_phyloseq = TRUE,
        simple_caption = TRUE,
        source = src,
        verbose = FALSE
      )
      expect_s4_class(result, "phyloseq")
    }
  })
})

# physeq + add_to_phyloseq=FALSE + gallery=TRUE
# (was broken: same unnamed taxa_match issue)
test_that("tax_photos_pq physeq + add_to_phyloseq=FALSE + gallery=TRUE returns shiny.tag", {
  skip_on_cran()
  physeq2 <- make_mini_physeq()
  for (src in c("gbif", "wikitaxa")) {
    result <- tax_photos_pq(
      physeq2,
      gallery = TRUE,
      add_to_phyloseq = FALSE,
      source = src,
      verbose = FALSE
    )
    expect_s3_class(result, "shiny.tag")
  }
})

# caption_valign options
test_that("tax_photos_pq caption_valign=top works", {
  skip_on_cran()
  result <- tax_photos_pq(
    taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
    gallery = TRUE,
    caption_valign = "top",
    verbose = FALSE
  )
  expect_s3_class(result, "shiny.tag")
})
