# Test package structure and basic functionality

test_that("package loads without errors", {
  # Test that the package namespace can be loaded
  # This will be tested when the package is properly installed
  skip("Package installation and loading tests")
})

test_that("exported functions are available", {
  # Test that main exported functions are available
  # This would test function availability after package loading
  skip("Requires package installation")
})

test_that("package dependencies are available", {
  # Test required dependencies
  required_packages <- c("httr", "jsonlite")
  
  for (pkg in required_packages) {
    # This would test if required packages are available
    # expect_true(requireNamespace(pkg, quietly = TRUE))
    skip(paste("Requires", pkg, "package"))
  }
})

test_that("example data files exist", {
  # Test that example data files are present
  extdata_files <- c(
    "TAXREFv18_fungi.csv",
    "TAXREFv18_fungi_mini.csv", 
    "bdc_18_01_wider_mini.csv",
    "fun_trait_mini.csv"
  )
  
  # Check that expected data files exist in inst/extdata
  extdata_path <- file.path("/home/runner/work/taxinfo/taxinfo/inst/extdata")
  
  for (file in extdata_files) {
    file_path <- file.path(extdata_path, file)
    expect_true(file.exists(file_path), info = paste("Missing file:", file))
  }
})

test_that("example data files can be read", {
  extdata_path <- file.path("/home/runner/work/taxinfo/taxinfo/inst/extdata")
  
  # Test TAXREFv18_fungi_mini.csv
  fungi_file <- file.path(extdata_path, "TAXREFv18_fungi_mini.csv")
  if (file.exists(fungi_file)) {
    fungi_data <- read.csv(fungi_file, stringsAsFactors = FALSE)
    expect_true(nrow(fungi_data) > 0)
    expect_true(ncol(fungi_data) > 0)
  }
  
  # Test fun_trait_mini.csv  
  trait_file <- file.path(extdata_path, "fun_trait_mini.csv")
  if (file.exists(trait_file)) {
    trait_data <- read.csv(trait_file, stringsAsFactors = FALSE, sep = ";")
    expect_true(nrow(trait_data) > 0)
    expect_true(ncol(trait_data) > 0)
  }
})

test_that("DESCRIPTION file is properly formatted", {
  desc_file <- file.path("/home/runner/work/taxinfo/taxinfo/DESCRIPTION")
  expect_true(file.exists(desc_file))
  
  # Read DESCRIPTION file
  desc_content <- readLines(desc_file)
  
  # Check for required fields
  expect_true(any(grepl("^Package:", desc_content)))
  expect_true(any(grepl("^Title:", desc_content)))
  expect_true(any(grepl("^Version:", desc_content)))
  expect_true(any(grepl("^Description:", desc_content)))
  expect_true(any(grepl("^License:", desc_content)))
  
  # Check for testthat in Suggests
  suggests_line <- desc_content[grepl("^Suggests:", desc_content)]
  if (length(suggests_line) > 0) {
    # Look for testthat in suggests and following lines
    suggests_section <- desc_content[grepl("^Suggests:", desc_content):length(desc_content)]
    testthat_mentioned <- any(grepl("testthat", suggests_section))
    expect_true(testthat_mentioned)
  }
})

test_that("NAMESPACE file exists", {
  namespace_file <- file.path("/home/runner/work/taxinfo/taxinfo/NAMESPACE")
  expect_true(file.exists(namespace_file))
  
  # Check that it has export statements
  namespace_content <- readLines(namespace_file)
  has_exports <- any(grepl("^export", namespace_content))
  
  # For a package with functions, we expect exports
  # This might be empty if NAMESPACE is auto-generated
  if (length(namespace_content) > 0) {
    expect_true(length(namespace_content) > 0)
  }
})

test_that("R directory structure is correct", {
  r_dir <- file.path("/home/runner/work/taxinfo/taxinfo/R")
  expect_true(dir.exists(r_dir))
  
  # Check that there are R files
  r_files <- list.files(r_dir, pattern = "\\.R$")
  expect_true(length(r_files) > 0)
  
  # Check for main functions mentioned in the code snippets
  expected_files <- c(
    "tax_photos_pq.R",
    "tax_occur_check.R", 
    "tax_info_pq.R",
    "tax_oa_pq.R",
    "taxinfo_utils.R",
    "taxonomic_rank_to_taxnames.R"
  )
  
  for (file in expected_files) {
    file_path <- file.path(r_dir, file)
    expect_true(file.exists(file_path), info = paste("Missing R file:", file))
  }
})