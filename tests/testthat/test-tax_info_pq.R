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
  skip_if_not_installed("dbplyr")
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

info_csv <- function(df) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  write.csv(df, path, row.names = FALSE)
  path
}

test_that("tax_info_pq keeps taxa_name and the tax_table column order", {
  csv <- info_csv(data.frame(
    sp = c("Fomes fomentarius", "Mycena renati"),
    trait = c("A", "B")
  ))
  res <- tax_info_pq(
    clean,
    file_name = csv,
    csv_taxonomic_rank = "sp",
    verbose = FALSE
  )
  old_cols <- colnames(clean@tax_table)
  expect_equal(colnames(res@tax_table), c(old_cols, "sp", "trait"))
  expect_equal(res@tax_table[, "taxa_name"], clean@tax_table[, "taxa_name"])
  expect_equal(
    as.vector(res@tax_table[, "trait"]),
    c(NA, "A", "B", "A", NA, NA, NA)
  )
  expect_equal(
    as.vector(res@tax_table[, "sp"])[1:3],
    c(NA, "Fomes fomentarius", "Mycena renati")
  )
})

test_that("tax_info_pq duckdb path matches the R path", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  csv <- info_csv(data.frame(
    sp = c("Fomes fomentarius", "Mycena renati"),
    trait = c("A", "B")
  ))
  res_r <- tax_info_pq(
    clean,
    file_name = csv,
    csv_taxonomic_rank = "sp",
    verbose = FALSE
  )
  res_duck <- tax_info_pq(
    clean,
    file_name = csv,
    csv_taxonomic_rank = "sp",
    use_duck_db = TRUE,
    verbose = FALSE
  )
  expect_equal(colnames(res_duck@tax_table), colnames(res_r@tax_table))
  expect_equal(res_duck@tax_table[, "trait"], res_r@tax_table[, "trait"])
})

test_that("tax_info_pq handles column collisions with the tax_table", {
  csv <- info_csv(data.frame(sp = "Fomes fomentarius", Guild = "csv_guild"))
  expect_error(
    tax_info_pq(
      clean,
      file_name = csv,
      csv_taxonomic_rank = "sp",
      verbose = FALSE
    ),
    "already exist"
  )
  res <- tax_info_pq(
    clean,
    file_name = csv,
    csv_taxonomic_rank = "sp",
    col_prefix = "x_",
    verbose = FALSE
  )
  expect_equal(res@tax_table[, "Guild"], clean@tax_table[, "Guild"])
  expect_equal(as.vector(res@tax_table[2, "x_Guild"]), "csv_guild")
})

test_that("tax_info_pq works with a two-column taxonomic_rank", {
  csv <- info_csv(data.frame(sp = "Fomes fomentarius", trait = "A"))
  expect_message(
    res <- tax_info_pq(
      clean,
      taxonomic_rank = c("Genus", "Species"),
      file_name = csv,
      csv_taxonomic_rank = "sp"
    ),
    "found for"
  )
  expect_equal(sum(!is.na(res@tax_table[, "trait"])), 2)
})

test_that("tax_info_pq with taxnames returns one row per name", {
  csv <- info_csv(data.frame(
    sp = c("Fomes fomentarius", "Fomes fomentarius"),
    trait = c("A", "Z")
  ))
  expect_message(
    res <- tax_info_pq(
      taxnames = c("Mycena renati", "Fomes fomentarius"),
      file_name = csv,
      csv_taxonomic_rank = "sp",
      col_prefix = "x_"
    ),
    "duplicated"
  )
  expect_equal(colnames(res), c("taxa_name", "x_sp", "x_trait"))
  expect_equal(res$x_trait, c(NA, "A"))
  expect_error(
    tax_info_pq(
      taxnames = "Fomes fomentarius",
      file_name = csv,
      csv_taxonomic_rank = "sp",
      csv_cols_select = "nope"
    ),
    "not in your CSV"
  )
})
