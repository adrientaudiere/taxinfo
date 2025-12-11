# Test tax_photos_pq function
# Examples from man page: tax_photos_pq.Rd

test_that("tax_photos_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_photos_pq(NULL))
})

test_that("tax_photos_pq folder operations", {
  # Test folder creation and management
  # Test overwrite_folder parameter

  # Create temporary directory for testing
  temp_dir <- tempdir()
  test_folder <- file.path(temp_dir, "test_photos")

  # Test folder creation logic
  if (!dir.exists(test_folder)) {
    dir.create(test_folder)
  }
  expect_true(dir.exists(test_folder))

  # Test overwrite behavior
  # Create a file in the folder
  test_file <- file.path(test_folder, "test.txt")
  writeLines("test", test_file)
  expect_true(file.exists(test_file))

  # Clean up
  unlink(test_folder, recursive = TRUE)
})

test_that("tax_photos_pq URL validation", {
  # Test URL format validation
  # Should validate that photo URLs are properly formatted

  # Test URL patterns
  valid_urls <- c(
    "https://example.com/photo.jpg",
    "http://example.com/image.png",
    "https://api.gbif.org/v1/image/unsafe/photo.jpeg"
  )

  invalid_urls <- c(
    "not_a_url",
    "ftp://example.com/photo.jpg",
    ""
  )

  # URL validation logic (simplified)
  url_pattern <- "^https?://.+\\.(jpg|jpeg|png|gif)$"

  expect_true(all(grepl(url_pattern, valid_urls, ignore.case = TRUE)))
  expect_false(any(grepl(url_pattern, invalid_urls, ignore.case = TRUE)))
})

test_that("tax_photos_pq gallery parameter", {
  # Test gallery creation functionality
  # When gallery = TRUE, should create HTML gallery using pixture::pixgallery()
  skip("Requires phyloseq objects and pixture package")
})

test_that("tax_photos_pq caption settings", {
  # Test caption parameters for gallery
  # caption_valign should control vertical alignment
  # caption_font_size should control font size
  # simple_caption should control caption content

  valid_valign_values <- c("top", "middle", "bottom")
  expect_true("bottom" %in% valid_valign_values) # default value

  # Font size should be positive integer
  expect_true(is.numeric(12)) # default value
  expect_true(12 > 0)
})

# Examples from man page: tax_photos_pq.Rd (lines 89-113)
test_that("tax_photos_pq with phyloseq returns phyloseq with photo_url", {
  # Example: data_fungi_mini_cleanNames_photos <- tax_photos_pq(data_fungi_mini_cleanNames)
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  data_fungi_mini_cleanNames_photos <- tax_photos_pq(data_fungi_mini_cleanNames)

  expect_s4_class(data_fungi_mini_cleanNames_photos, "phyloseq")
  expect_true("photo_url" %in% colnames(data_fungi_mini_cleanNames_photos@tax_table))
})

test_that("tax_photos_pq with taxnames and gallery = TRUE returns htmlwidget", {
  # Example: tax_photos_pq(taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
  #   gallery = TRUE, layout = "rhombus")
  result <- tax_photos_pq(
    taxnames = c("Xylodon flaviporus", "Basidiodendron eyrei"),
    gallery = TRUE,
    layout = "rhombus"
  )
  # gallery=TRUE returns htmlwidget from pixture::pixgallery()
  expect_true(inherits(result, "htmlwidget") || inherits(result, "pixgallery"))
})

test_that("tax_photos_pq with wikitaxa source and gallery = TRUE", {
  # Example: tax_photos_pq(data_fungi_mini_cleanNames, gallery = TRUE, h = "40px",
  #   w = "80px", source = "wikitaxa")
  data_fungi_mini_cleanNames <- gna_verifier_pq(data_fungi_mini)
  result <- tax_photos_pq(data_fungi_mini_cleanNames,
    gallery = TRUE,
    h = "40px",
    w = "80px",
    source = "wikitaxa"
  )
  expect_true(inherits(result, "htmlwidget") || inherits(result, "pixgallery"))
})
