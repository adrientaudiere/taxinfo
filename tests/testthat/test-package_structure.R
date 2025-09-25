test_that("example data files exist", {
  # Test that example data files are present
  extdata_files <- c(
    "TAXREFv18_fungi.csv",
    "TAXREFv18_fungi_mini.csv",
    "bdc_18_01_wider_mini.csv",
    "fun_trait_mini.csv"
  )

  # Check that expected data files exist in inst/extdata
  extdata_path <- file.path(paste0(system.file(package = "taxinfo"),"/extdata"))

  for (file in extdata_files) {
    file_path <- file.path(extdata_path, file)
    expect_true(file.exists(file_path), info = paste("Missing file:", file))
  }
})

test_that("example data files can be read", {
  extdata_path <- file.path(paste0(system.file(package = "taxinfo"),"/extdata"))

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
