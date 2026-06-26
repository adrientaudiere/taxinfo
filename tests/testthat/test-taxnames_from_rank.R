test_that("taxnames_from_rank pastes single and multi column ranks", {
  tt <- matrix(
    c("Amanita", "Boletus", "muscaria", "edulis"),
    ncol = 2,
    dimnames = list(c("t1", "t2"), c("Genus", "Species"))
  )
  expect_equal(
    taxnames_from_rank(tt, c("Genus", "Species")),
    c("Amanita muscaria", "Boletus edulis")
  )
  expect_equal(taxnames_from_rank(tt, "Genus"), c("Amanita", "Boletus"))
})

test_that("taxnames_from_rank cleans NA tokens when clean = TRUE", {
  tt <- matrix(
    c("Amanita", "Russula", NA, "muscaria", NA, NA),
    ncol = 2,
    dimnames = list(c("t1", "t2", "t3"), c("Genus", "Species"))
  )
  out <- taxnames_from_rank(tt, c("Genus", "Species"))
  expect_equal(out, c("Amanita muscaria", "Russula", ""))
  expect_false(any(grepl(" NA", out)))
})

test_that("taxnames_from_rank keeps raw NA tokens when clean = FALSE", {
  tt <- matrix(
    c("Amanita", NA),
    ncol = 2,
    dimnames = list("t1", c("Genus", "Species"))
  )
  expect_equal(
    taxnames_from_rank(tt, c("Genus", "Species"), clean = FALSE),
    "Amanita NA"
  )
})

test_that("taxnames_from_rank is one value per taxon, never dropped", {
  tt <- matrix(
    c(NA, "Amanita", NA, NA),
    ncol = 2,
    dimnames = list(c("t1", "t2"), c("Genus", "Species"))
  )
  out <- taxnames_from_rank(tt, c("Genus", "Species"))
  expect_length(out, 2)
  expect_equal(out, c("", "Amanita"))
})
