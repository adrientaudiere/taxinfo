# Simple test runner that doesn't require testthat package
# This allows running basic tests even without full R testing infrastructure

# Source the helper functions
source("tests/testthat/helper-test_utilities.R")

# Simple test framework
run_test <- function(test_name, test_function) {
  cat("Running test:", test_name, "\n")
  
  tryCatch({
    test_function()
    cat("  ✓ PASSED\n")
    return(TRUE)
  }, error = function(e) {
    cat("  ✗ FAILED:", e$message, "\n")
    return(FALSE)
  })
}

expect_true <- function(condition, info = "") {
  if (!condition) {
    stop(paste("Expected TRUE but got FALSE.", info))
  }
}

expect_false <- function(condition, info = "") {
  if (condition) {
    stop(paste("Expected FALSE but got TRUE.", info))
  }
}

expect_equal <- function(actual, expected, info = "") {
  if (!isTRUE(all.equal(actual, expected))) {
    stop(paste("Expected", deparse(expected), "but got", deparse(actual), ".", info))
  }
}

expect_error <- function(expr, info = "") {
  tryCatch({
    eval(expr)
    stop(paste("Expected error but none occurred.", info))
  }, error = function(e) {
    # Error occurred as expected
    invisible(TRUE)
  })
}

# Define test functions
test_coordinate_validation <- function() {
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
}

test_url_validation <- function() {
  # Test valid URLs
  expect_true(validate_url("https://example.com/photo.jpg"))
  expect_true(validate_url("http://example.com/image.png"))
  
  # Test invalid URLs
  expect_false(validate_url("not_a_url"))
  expect_false(validate_url(""))
  expect_false(validate_url(NULL))
  expect_false(validate_url(NA))
}

test_doi_validation <- function() {
  # Test valid DOIs
  expect_true(validate_doi("10.1000/182"))
  expect_true(validate_doi("10.1038/nature12373"))
  
  # Test invalid DOIs
  expect_false(validate_doi("not_a_doi"))
  expect_false(validate_doi(""))
  expect_false(validate_doi(NULL))
  expect_false(validate_doi(NA))
}

test_file_operations <- function() {
  # Test temporary CSV creation
  test_data <- data.frame(
    name = c("A", "B", "C"),
    value = c(1, 2, 3)
  )
  
  temp_file <- create_temp_csv(test_data)
  expect_true(file.exists(temp_file))
  
  # Read and verify
  read_data <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(nrow(read_data), 3)
  expect_equal(ncol(read_data), 2)
  
  # Clean up
  cleanup_temp_files(temp_file)
  expect_false(file.exists(temp_file))
}

test_mock_phyloseq <- function() {
  mock_physeq <- create_mock_phyloseq()
  
  expect_true(validate_phyloseq_structure(mock_physeq))
  expect_equal(nrow(mock_physeq$otu_table), 4)
  expect_equal(ncol(mock_physeq$otu_table), 3)
  expect_equal(nrow(mock_physeq$tax_table), 4)
  expect_equal(ncol(mock_physeq$tax_table), 8)
  expect_equal(nrow(mock_physeq$sample_data), 3)
}

test_package_structure <- function() {
  # Test that key directories exist
  expect_true(dir.exists("R"))
  expect_true(dir.exists("tests"))
  expect_true(dir.exists("inst/extdata"))
  
  # Test that key files exist
  expect_true(file.exists("DESCRIPTION"))
  expect_true(file.exists("NAMESPACE"))
  expect_true(file.exists("tests/testthat.R"))
  
  # Test R files
  r_files <- list.files("R", pattern = "[.]R$")
  expect_true(length(r_files) > 0)
  
  # Test example data files
  extdata_files <- list.files("inst/extdata", pattern = "[.]csv$")
  expect_true(length(extdata_files) > 0)
}

# Run all tests
cat("=== Running taxinfo Package Tests ===\n\n")

tests_passed <- 0
tests_total <- 0

tests <- list(
  "Coordinate Validation" = test_coordinate_validation,
  "URL Validation" = test_url_validation,
  "DOI Validation" = test_doi_validation,
  "File Operations" = test_file_operations,
  "Mock Phyloseq Creation" = test_mock_phyloseq,
  "Package Structure" = test_package_structure
)

for (test_name in names(tests)) {
  tests_total <- tests_total + 1
  if (run_test(test_name, tests[[test_name]])) {
    tests_passed <- tests_passed + 1
  }
  cat("\n")
}

cat("=== Test Summary ===\n")
cat("Passed:", tests_passed, "/", tests_total, "tests\n")

if (tests_passed == tests_total) {
  cat("All tests passed! ✓\n")
} else {
  cat("Some tests failed. ✗\n")
}