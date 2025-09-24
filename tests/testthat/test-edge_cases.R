# Integration tests and edge cases

test_that("coordinate edge cases", {
  # Test extreme coordinate values
  expect_true(validate_coordinates(-180, -90))  # SW corner
  expect_true(validate_coordinates(180, 90))    # NE corner
  expect_true(validate_coordinates(0, 0))       # Equator, Prime Meridian
  
  # Test boundary values
  expect_true(validate_coordinates(-179.99, -89.99))
  expect_true(validate_coordinates(179.99, 89.99))
  
  # Test just outside boundaries
  expect_false(validate_coordinates(-180.01, -90))
  expect_false(validate_coordinates(180.01, 90))
  expect_false(validate_coordinates(-180, -90.01))
  expect_false(validate_coordinates(180, 90.01))
})

test_that("URL edge cases", {
  # Test various URL formats
  edge_case_urls <- c(
    "https://example.com/path/to/image.jpg",
    "http://subdomain.example.com/image.png",
    "https://example.com/image.jpeg?param=value",
    "https://example.com/image.gif#fragment",
    "https://192.168.1.1/image.jpg",  # IP address
    "https://localhost:8080/image.png"  # localhost with port
  )
  
  for (url in edge_case_urls) {
    expect_true(validate_url(url), info = paste("Failed for URL:", url))
  }
  
  # Test edge cases that should fail
  edge_case_invalid <- c(
    "ftp://example.com/image.jpg",    # FTP protocol
    "https://",                       # Incomplete URL
    "https:///image.jpg",            # Missing domain
    "image.jpg",                     # No protocol
    "//example.com/image.jpg"        # Protocol-relative URL
  )
  
  for (url in edge_case_invalid) {
    expect_false(validate_url(url), info = paste("Should have failed for URL:", url))
  }
})

test_that("DOI edge cases", {
  # Test various DOI formats
  edge_case_dois <- c(
    "10.1000/1",                     # Minimal DOI
    "10.12345/very.long.suffix.with.many.parts",
    "10.1000/suffix-with-hyphens",
    "10.1000/suffix_with_underscores",
    "10.1000/123456789"              # Numeric suffix
  )
  
  for (doi in edge_case_dois) {
    expect_true(validate_doi(doi), info = paste("Failed for DOI:", doi))
  }
  
  # Test edge cases that should fail
  edge_case_invalid_dois <- c(
    "10.1000/",                      # No suffix
    "10./suffix",                    # No registrant
    "doi:10.1000/182",              # DOI: prefix (not standard for validation)
    "https://doi.org/10.1000/182",  # Full URL (not just DOI)
    "10"                            # Incomplete
  )
  
  for (doi in edge_case_invalid_dois) {
    expect_false(validate_doi(doi), info = paste("Should have failed for DOI:", doi))
  }
})

test_that("file operations edge cases", {
  # Test with various CSV formats
  
  # Test with semicolon separator
  test_data_semicolon <- data.frame(
    name = c("A", "B"),
    value = c(1, 2)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write.table(test_data_semicolon, temp_file, sep = ";", row.names = FALSE)
  
  expect_true(file.exists(temp_file))
  
  # Read with correct separator
  read_data <- read.table(temp_file, sep = ";", header = TRUE, stringsAsFactors = FALSE)
  expect_equal(nrow(read_data), 2)
  
  # Clean up
  unlink(temp_file)
  
  # Test with empty file
  empty_file <- tempfile(fileext = ".csv")
  writeLines("", empty_file)
  expect_true(file.exists(empty_file))
  unlink(empty_file)
  
  # Test with non-existent file
  non_existent <- tempfile(fileext = ".csv")
  expect_false(file.exists(non_existent))
})

test_that("taxonomic name processing edge cases", {
  # Test various taxonomic name formats
  mock_tax_edge_cases <- data.frame(
    Genus = c("Xylodon", "Basidiodendron", "", NA, "Genus with spaces"),
    Species = c("raduloides", "", "species", NA, "species_with_underscores"),
    stringsAsFactors = FALSE
  )
  
  # Test name combination
  combined_names <- paste(mock_tax_edge_cases$Genus, mock_tax_edge_cases$Species)
  
  expect_equal(length(combined_names), 5)
  
  # Test NA handling
  na_patterns <- grepl("NA", combined_names)
  expect_true(any(na_patterns))
  
  # Test empty string handling
  empty_patterns <- grepl("^\\s*$", combined_names)
  expect_false(all(empty_patterns))  # Not all should be empty
  
  # Test space handling in genus names
  space_genus <- combined_names[5]  # "Genus with spaces species_with_underscores"
  expect_true(grepl("Genus with spaces", space_genus))
})

test_that("parameter validation patterns", {
  # Test common parameter validation patterns used across functions
  
  # Test logical parameter validation
  valid_logical <- c(TRUE, FALSE)
  invalid_logical <- c("true", "false", 1, 0, NA, NULL)
  
  for (val in valid_logical) {
    expect_true(is.logical(val))
  }
  
  for (val in invalid_logical) {
    expect_false(is.logical(val))
  }
  
  # Test character parameter validation
  valid_character <- c("string", "", "with spaces", "with-hyphens", "with_underscores")
  invalid_character <- c(NA, NULL, 123, TRUE)
  
  for (val in valid_character) {
    expect_true(is.character(val))
  }
  
  for (val in invalid_character) {
    expect_false(is.character(val))
  }
  
  # Test numeric parameter validation
  valid_numeric <- c(0, 1, -1, 3.14, -3.14, Inf, -Inf)
  invalid_numeric <- c("123", TRUE, FALSE, NULL)
  
  for (val in valid_numeric) {
    expect_true(is.numeric(val))
  }
  
  for (val in invalid_numeric) {
    expect_false(is.numeric(val))
  }
})