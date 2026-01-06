# Test check_package function
# This tests the check_package utility function from taxinfo_utils.R

test_that("check_package validates input parameter types", {
  # Test that package must be a single character string
  expect_error(check_package(NULL), "must be a single character string")
  expect_error(check_package(123), "must be a single character string")
  expect_error(check_package(c("pkg1", "pkg2")), "must be a single character string")
})

test_that("check_package returns TRUE for installed packages", {
  # Test with a package that should always be installed
  expect_true(check_package("base", stop_on_error = FALSE))
  expect_true(check_package("stats", stop_on_error = FALSE))
})

test_that("check_package handles stop_on_error parameter correctly", {
  # Test with a non-existent package
  nonexistent_pkg <- "this_package_definitely_does_not_exist_12345"

  # Should return FALSE when stop_on_error = FALSE
  expect_false(check_package(nonexistent_pkg, stop_on_error = FALSE))

  # Should error when stop_on_error = TRUE (default)
  expect_error(check_package(nonexistent_pkg, stop_on_error = TRUE))
})

test_that("check_package handles repo parameter correctly", {
  nonexistent_pkg <- "nonexistent_test_package_xyz"

  # Test CRAN repo
  expect_false(check_package(nonexistent_pkg, repo = "CRAN", stop_on_error = FALSE))

  # Test Bioconductor repo
  expect_false(check_package(nonexistent_pkg, repo = "Bioconductor", stop_on_error = FALSE))
})

test_that("check_package github_repo parameter overrides repo",
{
  nonexistent_pkg <- "nonexistent_test_package_xyz"

  # Test that github_repo overrides repo to "GitHub"
  expect_false(check_package(nonexistent_pkg,
    repo = "CRAN",
    github_repo = "user/repo",
    stop_on_error = FALSE
  ))
})

test_that("check_package errors when GitHub repo without github_repo parameter", {
  nonexistent_pkg <- "nonexistent_test_package_xyz"

  # Should error when repo is "GitHub" but github_repo is NULL
  expect_error(
    check_package(nonexistent_pkg, repo = "GitHub", stop_on_error = TRUE),
    "github_repo"
  )
})

test_that("check_package quietly parameter works", {
  # Test that quietly parameter is passed to requireNamespace
  expect_true(check_package("base", quietly = TRUE, stop_on_error = FALSE))
  expect_true(check_package("base", quietly = FALSE, stop_on_error = FALSE))
})
