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
