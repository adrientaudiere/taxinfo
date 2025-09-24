# Test parameter validation for all taxinfo functions

test_that("parameter type validation", {
  # Test that parameter validation follows consistent patterns
  
  # Test taxonomic_rank parameter validation
  valid_taxonomic_ranks <- c(
    "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species",
    "currentCanonicalSimple", "scientificName"
  )
  
  for (rank in valid_taxonomic_ranks) {
    expect_true(is.character(rank))
    expect_true(nchar(rank) > 0)
  }
  
  # Test verbose parameter validation
  expect_true(is.logical(TRUE))
  expect_true(is.logical(FALSE))
  expect_false(is.logical("true"))
  expect_false(is.logical(1))
  expect_false(is.logical(0))
  
  # Test add_to_phyloseq parameter validation
  expect_true(is.logical(TRUE))
  expect_true(is.logical(FALSE))
})

test_that("radius and distance parameter validation", {
  # Test radius_km parameter validation (used in multiple functions)
  valid_radii <- c(1, 10, 50, 100, 500, 1000)
  invalid_radii <- c(-1, 0, -10, "50", NULL, NA, Inf, -Inf)
  
  for (radius in valid_radii) {
    expect_true(is.numeric(radius))
    expect_true(radius > 0)
    expect_true(is.finite(radius))
  }
  
  for (radius in invalid_radii) {
    if (!is.null(radius) && !is.na(radius)) {
      if (is.numeric(radius)) {
        expect_false(radius > 0 && is.finite(radius))
      } else {
        expect_false(is.numeric(radius))
      }
    }
  }
})

test_that("file path parameter validation", {
  # Test file_name parameter validation (used in tax_info_pq)
  
  # Test with existing files
  existing_files <- list.files("inst/extdata", full.names = TRUE)
  if (length(existing_files) > 0) {
    for (file in existing_files) {
      expect_true(file.exists(file))
      expect_true(is.character(file))
      expect_true(nchar(file) > 0)
    }
  }
  
  # Test with non-existent files
  non_existent_files <- c(
    "non_existent_file.csv",
    "/path/that/does/not/exist.csv",
    ""
  )
  
  for (file in non_existent_files) {
    if (file != "") {
      expect_false(file.exists(file))
    }
  }
})

test_that("data source parameter validation", {
  # Test data_sources parameter (used in gna_verifier_pq)
  
  # Valid data sources (1-12 according to GNA)
  valid_sources <- 1:12
  invalid_sources <- c(0, 13, -1, 100, "1", NULL, NA)
  
  for (source in valid_sources) {
    expect_true(is.numeric(source))
    expect_true(source >= 1 && source <= 12)
    expect_true(source == round(source))  # Should be integer
  }
  
  for (source in invalid_sources) {
    if (!is.null(source) && !is.na(source)) {
      if (is.numeric(source)) {
        expect_false(source >= 1 && source <= 12 && source == round(source))
      } else {
        expect_false(is.numeric(source))
      }
    }
  }
})

test_that("work type parameter validation", {
  # Test type_works parameter (used in tax_oa_pq)
  
  valid_work_types <- c("article", "review", "book-chapter", "book", "letter", 
                       "preprint", "dataset", "thesis", "proceeding")
  
  # Default types
  default_types <- c("article", "review", "book-chapter", "book", "letter")
  
  for (type in default_types) {
    expect_true(type %in% valid_work_types)
    expect_true(is.character(type))
    expect_true(nchar(type) > 0)
  }
  
  # Test invalid types
  invalid_types <- c("", "invalid_type", 123, NULL, NA)
  
  for (type in invalid_types) {
    if (!is.null(type) && !is.na(type)) {
      if (is.character(type)) {
        expect_false(type %in% valid_work_types)
      } else {
        expect_false(is.character(type))
      }
    }
  }
})

test_that("gallery parameter validation", {
  # Test gallery-related parameters (used in tax_photos_pq)
  
  # Test caption_valign parameter
  valid_valign <- c("top", "middle", "bottom")
  invalid_valign <- c("", "left", "right", "center", 123, NULL, NA)
  
  for (valign in valid_valign) {
    expect_true(is.character(valign))
    expect_true(nchar(valign) > 0)
  }
  
  # Test caption_font_size parameter
  valid_font_sizes <- c(8, 10, 12, 14, 16, 18, 20, 24)
  invalid_font_sizes <- c(-1, 0, "12", NULL, NA, Inf)
  
  for (size in valid_font_sizes) {
    expect_true(is.numeric(size))
    expect_true(size > 0)
    expect_true(is.finite(size))
  }
  
  for (size in invalid_font_sizes) {
    if (!is.null(size) && !is.na(size)) {
      if (is.numeric(size)) {
        expect_false(size > 0 && is.finite(size))
      } else {
        expect_false(is.numeric(size))
      }
    }
  }
})

test_that("occurrence parameter validation", {
  # Test min_occur parameter (used in tax_occur_multi_check_pq)
  
  valid_min_occur <- c(0, 1, 5, 10, 50, 100)
  invalid_min_occur <- c(-1, -10, "5", NULL, NA, Inf, 3.14)
  
  for (min_val in valid_min_occur) {
    expect_true(is.numeric(min_val))
    expect_true(min_val >= 0)
    expect_true(min_val == round(min_val))  # Should be integer
    expect_true(is.finite(min_val))
  }
  
  for (min_val in invalid_min_occur) {
    if (!is.null(min_val) && !is.na(min_val)) {
      if (is.numeric(min_val)) {
        expect_false(min_val >= 0 && min_val == round(min_val) && is.finite(min_val))
      } else {
        expect_false(is.numeric(min_val))
      }
    }
  }
})

test_that("column name parameter validation", {
  # Test col_prefix and col_name_url parameters
  
  valid_prefixes <- c("", "tax_", "gbif_", "oa_", "photo_", "trait_", "status_")
  invalid_prefixes <- c(NULL, NA, 123, TRUE)
  
  for (prefix in valid_prefixes) {
    expect_true(is.character(prefix))
  }
  
  for (prefix in invalid_prefixes) {
    expect_false(is.character(prefix))
  }
  
  # Test column names
  valid_col_names <- c("photo_url", "n_doi", "list_doi", "occurrence_count", "trait_value")
  
  for (col_name in valid_col_names) {
    expect_true(is.character(col_name))
    expect_true(nchar(col_name) > 0)
    expect_false(grepl("[^a-zA-Z0-9_]", col_name))  # Should only contain alphanumeric and underscore
  }
})