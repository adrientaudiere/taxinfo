# Test helper functions

test_that("create_mock_phyloseq creates valid structure", {
  mock_physeq <- create_mock_phyloseq()

  expect_true(validate_phyloseq_structure(mock_physeq))
  expect_true(is.matrix(mock_physeq$otu_table))
  expect_true(is.matrix(mock_physeq$tax_table))
  expect_true(is.data.frame(mock_physeq$sample_data))

  # Check dimensions
  expect_equal(nrow(mock_physeq$otu_table), 4) # 4 OTUs
  expect_equal(ncol(mock_physeq$otu_table), 3) # 3 samples
  expect_equal(nrow(mock_physeq$tax_table), 4) # 4 OTUs
  expect_equal(ncol(mock_physeq$tax_table), 8) # 8 taxonomic ranks
  expect_equal(nrow(mock_physeq$sample_data), 3) # 3 samples
})

test_that("validate_coordinates works correctly", {
  # Test valid coordinates
  expect_true(validate_coordinates(2.3522, 48.8566))
  expect_true(validate_coordinates(-180, -90))
  expect_true(validate_coordinates(180, 90))
  expect_true(validate_coordinates(0, 0))

  # Test invalid coordinates
  expect_false(validate_coordinates(NULL, 48.8566))
  expect_false(validate_coordinates(2.3522, NULL))
  expect_false(validate_coordinates(-200, 48.8566))
  expect_false(validate_coordinates(200, 48.8566))
  expect_false(validate_coordinates(2.3522, -100))
  expect_false(validate_coordinates(2.3522, 100))
  expect_false(validate_coordinates("invalid", 48.8566))
  expect_false(validate_coordinates(2.3522, "invalid"))
})

test_that("validate_url works correctly", {
  # Test valid URLs
  for (url in TEST_URLS$valid) {
    expect_true(validate_url(url), info = paste("Failed for URL:", url))
  }

  # Test invalid URLs
  for (url in TEST_URLS$invalid) {
    expect_false(validate_url(url), info = paste("Failed for URL:", url))
  }
})

test_that("validate_doi works correctly", {
  # Test valid DOIs
  for (doi in TEST_DOIS$valid) {
    expect_true(validate_doi(doi), info = paste("Failed for DOI:", doi))
  }

  # Test invalid DOIs
  for (doi in TEST_DOIS$invalid) {
    expect_false(validate_doi(doi), info = paste("Failed for DOI:", doi))
  }
})

test_that("create_temp_csv and cleanup_temp_files work", {
  # Create test data
  test_data <- data.frame(
    name = c("A", "B", "C"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  # Create temporary CSV
  temp_file <- create_temp_csv(test_data)
  expect_true(file.exists(temp_file))

  # Read and verify content
  read_data <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(nrow(read_data), 3)
  expect_equal(ncol(read_data), 2)
  expect_true("name" %in% colnames(read_data))
  expect_true("value" %in% colnames(read_data))

  # Clean up
  cleanup_temp_files(temp_file)
  expect_false(file.exists(temp_file))
})

test_that("TEST_COORDINATES constants are valid", {
  # Test valid coordinates
  for (i in seq_along(TEST_COORDINATES$valid$longitude)) {
    lon <- TEST_COORDINATES$valid$longitude[i]
    lat <- TEST_COORDINATES$valid$latitude[i]
    if (!is.na(lon) && !is.na(lat)) {
      expect_true(validate_coordinates(lon, lat))
    }
  }

  # Test that we have both valid and invalid examples
  expect_true(length(TEST_COORDINATES$valid$longitude) > 0)
  expect_true(length(TEST_COORDINATES$invalid$longitude) > 0)
})

test_that("TEST_URLS constants are valid", {
  expect_true(length(TEST_URLS$valid) > 0)
  expect_true(length(TEST_URLS$invalid) > 0)

  # At least one valid URL should be HTTPS
  https_urls <- grepl("^https://", TEST_URLS$valid)
  expect_true(any(https_urls))
})

test_that("TEST_DOIS constants are valid", {
  expect_true(length(TEST_DOIS$valid) > 0)
  expect_true(length(TEST_DOIS$invalid) > 0)

  # All valid DOIs should start with "10."
  valid_patterns <- grepl("^10\\.", TEST_DOIS$valid)
  expect_true(all(valid_patterns))
})
