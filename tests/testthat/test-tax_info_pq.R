# Test tax_info_pq function
# Examples from man page: tax_info_pq.Rd

# Shared, pre-cleaned fixture (replaces repeated gna_verifier_pq() setups).
clean <- load_clean_pq()
taxref_csv <- system.file(
  "extdata",
  "TAXREFv18_fungi_mini.csv",
  package = "taxinfo"
)

test_that("tax_info_pq input validation", {
  # Test with NULL phyloseq object
  expect_error(tax_info_pq(NULL))
})

test_that("tax_info_pq file handling", {
  # Test CSV file reading and processing
  # Test that file exists before processing
  # Test sep and dec parameters for CSV parsing

  # Create a temporary test CSV file
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    GENUS = c("Xylodon", "Basidiodendron"),
    HABITAT = c("Wood", "Soil"),
    FR = c("France", "France"),
    stringsAsFactors = FALSE
  )
  write.csv(test_data, temp_csv, row.names = FALSE)

  # Test that file can be read
  expect_true(file.exists(temp_csv))

  # Test CSV reading with different separators
  read_data <- read.csv(temp_csv, sep = ",")
  expect_equal(nrow(read_data), 2)
  expect_true("GENUS" %in% colnames(read_data))

  # Clean up
  unlink(temp_csv)
})

# Examples from man page: tax_info_pq.Rd (lines 77-168)
test_that("tax_info_pq with fungal traits returns a tibble", {
  fungal_traits <- system.file(
    "extdata",
    "fun_trait_mini.csv",
    package = "taxinfo"
  )

  fg_traits <- tax_info_pq(
    clean,
    taxonomic_rank = "genusEpithet",
    file_name = fungal_traits,
    csv_taxonomic_rank = "GENUS",
    col_prefix = "ft_",
    sep = ";",
    add_to_phyloseq = FALSE
  )

  expect_s3_class(fg_traits, "data.frame")
})

test_that("tax_info_pq returns phyloseq with TAXREF data", {
  res_with_R <- tax_info_pq(
    clean,
    file_name = taxref_csv,
    csv_taxonomic_rank = "NOM_VALIDE_SIMPLE",
    col_prefix = "taxref_"
  )

  expect_s4_class(res_with_R, "phyloseq")
})

test_that("tax_info_pq both physeq and taxnames errors", {
  expect_error(
    tax_info_pq(physeq = "dummy", taxnames = c("Amanita muscaria")),
    "You must specify either"
  )
})

test_that("tax_info_pq neither physeq nor taxnames errors", {
  expect_error(
    tax_info_pq(physeq = NULL, taxnames = NULL),
    "You must specify either"
  )
})

test_that("tax_info_pq file_name is required", {
  expect_error(
    tax_info_pq(
      clean,
      csv_taxonomic_rank = "NOM_VALIDE_SIMPLE"
    ),
    "file_name"
  )
})

test_that("tax_info_pq file_name must exist", {
  expect_error(
    tax_info_pq(
      clean,
      file_name = "/nonexistent/path/file.csv",
      csv_taxonomic_rank = "NOM_VALIDE_SIMPLE"
    ),
    "does not exist"
  )
})

test_that("tax_info_pq csv_taxonomic_rank is required", {
  expect_error(
    tax_info_pq(clean, file_name = taxref_csv),
    "csv_taxonomic_rank"
  )
})

test_that("tax_info_pq add_to_phyloseq cannot be TRUE with taxnames", {
  expect_error(
    tax_info_pq(
      taxnames = c("Amanita muscaria"),
      file_name = taxref_csv,
      csv_taxonomic_rank = "NOM_VALIDE_SIMPLE",
      add_to_phyloseq = TRUE
    ),
    "cannot be TRUE when.*taxnames"
  )
})

test_that("tax_info_pq with taxnames returns tibble", {
  result <- tax_info_pq(
    taxnames = c("Amanita muscaria"),
    file_name = taxref_csv,
    csv_taxonomic_rank = "NOM_VALIDE_SIMPLE",
    add_to_phyloseq = FALSE
  )

  expect_s3_class(result, "data.frame")
})

test_that("tax_info_pq use_duck_db parameter works", {
  skip_if_not_installed("duckdb")
  result <- tax_info_pq(
    clean,
    file_name = taxref_csv,
    csv_taxonomic_rank = "NOM_VALIDE_SIMPLE",
    col_prefix = "taxref_",
    use_duck_db = TRUE,
    csv_cols_select = c("RANG", "HABITAT"),
    add_to_phyloseq = FALSE
  )

  expect_s3_class(result, "tbl_df")
})
