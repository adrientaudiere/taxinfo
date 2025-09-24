# Testing Documentation for taxinfo Package

## Overview

This directory contains comprehensive tests for the `taxinfo` package, which augments phyloseq objects with taxonomic information from various sources including GBIF, OpenAlex, WikiTaxa, and other databases.

## Test Structure

### Test Files

1. **`test-taxinfo_utils.R`** - Tests for utility functions
   - `calculate_bbox()` - Geographic bounding box calculation
   - `taxa_summary_text()` - Taxonomic summary text generation

2. **`test-taxonomic_rank_to_taxnames.R`** - Tests for taxonomic name extraction
   - Parameter validation and default handling
   - NA value processing
   - Genus-species combination logic

3. **`test-select_taxa_pq.R`** - Tests for taxa selection functionality
   - Input validation for phyloseq objects
   - Taxa filtering logic

4. **`test-tax_occur_check.R`** - Tests for GBIF occurrence checking
   - Coordinate validation (longitude/latitude ranges)
   - Parameter defaults and validation
   - Bounding box integration
   - Return structure validation

5. **`test-tax_info_pq.R`** - Tests for taxonomic information integration
   - CSV file handling with different separators
   - DuckDB parameter functionality
   - Column prefix handling
   - Return behavior (phyloseq vs tibble)

6. **`test-tax_photos_pq.R`** - Tests for photo URL handling
   - URL validation and format checking
   - Gallery parameter validation
   - Source parameter validation (GBIF vs WikiTaxa)
   - Folder operations and overwrite behavior

7. **`test-tax_oa_pq.R`** - Tests for OpenAlex integration
   - DOI validation and format checking
   - Publication type validation
   - Parameter combination validation
   - Return structure validation

8. **`test-tax_occur_multi_check_pq.R`** - Tests for multi-location occurrence checking
   - Coordinate vector validation
   - Unique coordinate pair processing
   - Minimum occurrence filtering

9. **`test-gna_verifier_pq.R`** - Tests for Global Names Architecture integration
   - Data source validation (1-12)
   - Name verification logic

10. **`test-plot_tax_gbif_pq.R`** - Tests for GBIF plotting functionality
    - Basic plotting parameter validation

11. **`test-tax_gbif_occur_pq.R`** - Tests for GBIF occurrence data retrieval
    - Parameter defaults and validation

12. **`test-additional_functions.R`** - Tests for remaining package functions
    - Input validation patterns
    - Common error handling
    - Parameter default validation

13. **`test-package_structure.R`** - Tests for package structure
    - File and directory existence
    - DESCRIPTION file validation
    - Example data file validation
    - NAMESPACE validation

14. **`test-helper_functions.R`** - Tests for test helper functions
    - Mock data creation validation
    - Validation function testing
    - Constant validation

15. **`test-edge_cases.R`** - Tests for edge cases and boundary conditions
    - Extreme coordinate values
    - Various URL formats
    - DOI format variations
    - File operation edge cases

16. **`test-parameter_validation.R`** - Comprehensive parameter validation tests
    - Type validation patterns
    - Range validation
    - Format validation

### Helper Files

- **`helper-test_utilities.R`** - Helper functions and constants
  - Mock phyloseq object creation
  - Validation functions for coordinates, URLs, DOIs
  - Temporary file management
  - Test constants and data

## Test Categories

### Input Validation Tests
- NULL input handling
- Parameter type checking
- Range validation (coordinates, radii)
- Format validation (URLs, DOIs)

### Parameter Default Tests
- Verification of documented default values
- Parameter combination validation
- Mutually exclusive parameter checking

### Edge Case Tests
- Boundary value testing
- Extreme input handling
- Format variation testing
- Error condition testing

### Integration Tests (Skipped)
- API integration tests (GBIF, OpenAlex, WikiTaxa)
- Phyloseq object integration
- External package dependencies

## Running Tests

### With testthat (recommended)
```r
library(testthat)
test_dir("tests/testthat")
```

### With simple test runner (no dependencies)
```r
source("simple_test_runner.R")
```

### Individual test files
```r
source("tests/testthat/helper-test_utilities.R")
source("tests/testthat/test-taxinfo_utils.R")
# Run individual test functions...
```

## Test Philosophy

### What We Test
- **Input validation**: All functions properly validate inputs
- **Parameter handling**: Default values and parameter combinations
- **Error handling**: Appropriate error messages for invalid inputs
- **Edge cases**: Boundary conditions and extreme values
- **File operations**: CSV reading, temporary file handling
- **Data validation**: URL, DOI, coordinate format checking

### What We Skip
- **External API calls**: Tests requiring GBIF, OpenAlex, WikiTaxa APIs
- **Package dependencies**: Tests requiring phyloseq, MiscMetabar packages
- **Network operations**: Any functionality requiring internet access
- **Complex integrations**: Full end-to-end workflows

### Mocking Strategy
- Mock phyloseq objects with realistic structure
- Mock data for testing file operations
- Validation functions for format checking
- Constants for testing edge cases

## Coverage

The test suite provides comprehensive coverage of:
- ✅ All utility functions
- ✅ Input validation for all main functions
- ✅ Parameter validation and defaults
- ✅ Edge cases and error handling
- ✅ File operations and data handling
- ✅ Package structure validation
- ⏭️ External API integrations (skipped)
- ⏭️ Complex phyloseq operations (skipped)

## Contributing to Tests

When adding new functions or modifying existing ones:

1. Add input validation tests
2. Test parameter defaults and validation
3. Add edge case tests
4. Update helper functions if needed
5. Document any new test patterns
6. Ensure tests can run without external dependencies

## Notes

- Tests use `skip()` for functionality requiring external dependencies
- Helper functions provide framework for integration testing when dependencies become available
- Simple test runner allows basic validation without testthat package
- All tests focus on the core logic and validation that can be tested independently