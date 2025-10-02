# tests/testthat/test-selection.R

testthat::test_that("update_selection: adds, toggles, de-duplicates, enforces max 2", {
  # start empty, add one
  ids <- character()
  ids <- update_selection(ids, "105")
  testthat::expect_identical(ids, c("105"))

  # add second
  ids <- update_selection(ids, "Local")
  testthat::expect_identical(ids, c("105", "Local"))

  # add third -> keeps the most recent two (tail)
  ids <- update_selection(ids, "103")
  testthat::expect_identical(ids, c("Local", "103"))

  # clicking an existing id removes it (toggle off)
  ids <- update_selection(ids, "Local")
  testthat::expect_identical(ids, c("103"))

  # adding a duplicate doesn't create another copy
  ids <- update_selection(ids, "103")
  testthat::expect_identical(ids, character())
})

testthat::test_that("update_selection: preserves first-occurrence order before truncation", {
  ids <- c("Local", "110")
  ids <- update_selection(ids, "105") # => c("Local","110","105") -> tail 2
  testthat::expect_identical(ids, c("110", "105")) # last two kept in order they appeared
})

testthat::test_that("update_selection: accepts non-character & invalid clicks gracefully", {
  # numeric click coerces to character
  ids <- update_selection(character(), 105)
  testthat::expect_identical(ids, "105")
  testthat::expect_true(is.character(ids))

  # empty string: no-op
  ids2 <- update_selection(ids, "")
  testthat::expect_identical(ids2, ids)

  # NA: no-op
  ids3 <- update_selection(ids, NA_character_)
  testthat::expect_identical(ids3, ids)
})

testthat::test_that("update_selection: configurable max_sel works", {
  ids <- character()
  ids <- update_selection(ids, "101", max_sel = 3L)
  ids <- update_selection(ids, "102", max_sel = 3L)
  ids <- update_selection(ids, "103", max_sel = 3L)
  testthat::expect_identical(ids, c("101", "102", "103"))

  # adding a 4th keeps the last three
  ids <- update_selection(ids, "104", max_sel = 3L)
  testthat::expect_identical(ids, c("102", "103", "104"))
})

testthat::test_that("compute_selection: returns NULLs when fewer than two selections", {
  s1 <- compute_selection(character())
  testthat::expect_true(is.null(s1$prior))
  testthat::expect_true(is.null(s1$newer))
  testthat::expect_identical(s1$ids, character())

  s2 <- compute_selection("Local")
  testthat::expect_true(is.null(s2$prior))
  testthat::expect_true(is.null(s2$newer))
  testthat::expect_identical(s2$ids, "Local")

  s3 <- compute_selection("abc")
  testthat::expect_true(is.null(s3$prior))
  testthat::expect_true(is.null(s3$newer))
  testthat::expect_identical(s3$ids, "abc")
})

testthat::test_that("compute_selection: Local + one numeric => prior numeric, newer NULL", {
  s <- compute_selection(c("105", "Local"))
  testthat::expect_identical(s$prior, 105)
  testthat::expect_true(is.null(s$newer))
  testthat::expect_identical(s$ids, c("105", "Local"))
})

testthat::test_that("compute_selection: Local + multiple numerics => prior is min(other), newer NULL", {
  s <- compute_selection(c("Local", "110", "105", "107"))
  testthat::expect_identical(s$prior, 105)
  testthat::expect_true(is.null(s$newer))
  testthat::expect_identical(s$ids, c("Local", "110", "105", "107"))
})

testthat::test_that("compute_selection: two numerics => prior/newer are the two smallest", {
  s <- compute_selection(c("110", "105"))
  testthat::expect_identical(s$prior, 105)
  testthat::expect_identical(s$newer, 110)
  testthat::expect_identical(s$ids, c("110", "105"))
})

testthat::test_that("compute_selection: >2 numerics => uses the two smallest", {
  s <- compute_selection(c("110", "105", "107", "200"))
  testthat::expect_identical(s$prior, 105)
  testthat::expect_identical(s$newer, 107)
  testthat::expect_identical(s$ids, c("110", "105", "107", "200"))
})

testthat::test_that("compute_selection: ignores non-numeric strings (besides 'Local') for pairing", {
  s <- compute_selection(c("foo", "105", "bar", "107"))
  testthat::expect_identical(s$prior, 105)
  testthat::expect_identical(s$newer, 107)
  testthat::expect_identical(s$ids, c("foo", "105", "bar", "107"))
})

testthat::test_that("compute_selection: de-duplicates while preserving first occurrences", {
  s <- compute_selection(c("Local", "105", "105", "Local", "107", "107"))
  testthat::expect_identical(s$ids, c("Local", "105", "107"))
  testthat::expect_identical(s$prior, 105)
  testthat::expect_true(is.null(s$newer)) # Local present => newer is NULL
})

testthat::test_that("integration: update_selection + compute_selection behave as intended", {
  ids <- character()
  ids <- update_selection(ids, "105")
  ids <- update_selection(ids, "Local")
  # adding a third keeps last 2
  ids <- update_selection(ids, "103")
  testthat::expect_identical(ids, c("Local", "103"))

  sel <- compute_selection(ids)
  testthat::expect_identical(sel$prior, 103)
  testthat::expect_true(is.null(sel$newer)) # Local implies newer is local
})
