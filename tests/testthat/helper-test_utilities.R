# Test helper functions and setup

# Helper function to create mock phyloseq objects
create_mock_phyloseq <- function() {
  # This function would create a minimal mock phyloseq object for testing
  # when phyloseq package becomes available

  # Mock OTU table
  otu_matrix <- matrix(
    data = c(10, 5, 8, 12, 15, 3, 7, 9, 6, 11, 4, 13),
    nrow = 4, ncol = 3,
    dimnames = list(
      c("OTU1", "OTU2", "OTU3", "OTU4"),
      c("Sample1", "Sample2", "Sample3")
    )
  )

  # Mock taxonomy table
  tax_matrix <- matrix(
    data = c(
      "Fungi", "Basidiomycota", "Agaricomycetes", "Polyporales", "Polyporaceae", "Xylodon", "raduloides", "Xylodon raduloides",
      "Fungi", "Basidiomycota", "Tremellomycetes", "Tremellales", "Tremellaceae", "Basidiodendron", "eyrei", "Basidiodendron eyrei",
      "Fungi", "Ascomycota", "Sordariomycetes", "Hypocreales", "Nectriaceae", "Fusarium", "oxysporum", "Fusarium oxysporum",
      "Fungi", "Basidiomycota", "Agaricomycetes", "Polyporales", "Polyporaceae", "Trametes", "versicolor", "Trametes versicolor"
    ),
    nrow = 4, ncol = 8,
    dimnames = list(
      c("OTU1", "OTU2", "OTU3", "OTU4"),
      c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "currentCanonicalSimple")
    )
  )

  # Mock sample data
  sample_df <- data.frame(
    Sample_ID = c("Sample1", "Sample2", "Sample3"),
    Location = c("Forest_A", "Forest_B", "Forest_C"),
    Longitude = c(2.3522, 3.0, 4.5),
    Latitude = c(48.8566, 49.0, 50.2),
    stringsAsFactors = FALSE
  )
  rownames(sample_df) <- c("Sample1", "Sample2", "Sample3")

  # Return mock data structure
  list(
    otu_table = otu_matrix,
    tax_table = tax_matrix,
    sample_data = sample_df
  )
}

# Helper function to validate phyloseq object structure
validate_phyloseq_structure <- function(physeq_mock) {
  # Validate that mock phyloseq has expected components
  expected_components <- c("otu_table", "tax_table", "sample_data")

  has_components <- names(physeq_mock) %in% expected_components
  all(has_components)
}

# Helper function to test coordinate validation
validate_coordinates <- function(longitude, latitude) {
  if (is.null(longitude) || is.null(latitude)) {
    return(FALSE)
  }

  if (!is.numeric(longitude) || !is.numeric(latitude)) {
    return(FALSE)
  }

  if (longitude < -180 || longitude > 180) {
    return(FALSE)
  }

  if (latitude < -90 || latitude > 90) {
    return(FALSE)
  }

  return(TRUE)
}

# Helper function to test URL validation
validate_url <- function(url) {
  if (is.null(url) || is.na(url) || url == "") {
    return(FALSE)
  }

  # Basic URL pattern matching
  url_pattern <- "^https?://.+"
  grepl(url_pattern, url, ignore.case = TRUE)
}

# Helper function to test DOI validation
validate_doi <- function(doi) {
  if (is.null(doi) || is.na(doi) || doi == "") {
    return(FALSE)
  }

  # Basic DOI pattern matching
  doi_pattern <- "^10\\.[0-9]+/.+"
  grepl(doi_pattern, doi)
}

# Helper function to create temporary test files
create_temp_csv <- function(data, filename = NULL) {
  if (is.null(filename)) {
    filename <- tempfile(fileext = ".csv")
  }

  write.csv(data, filename, row.names = FALSE)
  return(filename)
}

# Helper function to clean up temp files
cleanup_temp_files <- function(filenames) {
  for (filename in filenames) {
    if (file.exists(filename)) {
      unlink(filename)
    }
  }
}

# Constants for testing
TEST_COORDINATES <- list(
  valid = list(
    longitude = c(-180, -90, 0, 90, 180, 2.3522),
    latitude = c(-90, -45, 0, 45, 90, 48.8566)
  ),
  invalid = list(
    longitude = c(-200, 200, NA, NULL),
    latitude = c(-100, 100, NA, NULL)
  )
)

TEST_URLS <- list(
  valid = c(
    "https://example.com/photo.jpg",
    "http://example.com/image.png",
    "https://api.gbif.org/v1/image/unsafe/photo.jpeg"
  ),
  invalid = c(
    "not_a_url",
    "ftp://example.com/photo.jpg",
    "",
    NA
  )
)

TEST_DOIS <- list(
  valid = c(
    "10.1000/182",
    "10.1038/nature12373",
    "10.1371/journal.pone.0000000"
  ),
  invalid = c(
    "not_a_doi",
    "10.invalid",
    "",
    NA
  )
)
