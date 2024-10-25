# Tests for qcOutstanding
test_that("qcOutstanding returns correct data frame", {
  # Create temporary test files
  tf1 <- tempfile(fileext = ".R")
  tf2 <- tempfile(fileext = ".R")
  
  writeLines(c(
    "# Regular comment",
    'qcComment("check this")',
    "x <- 1",
    'review::qcComment("another check")'
  ), tf1)
  
  writeLines(c(
    "y <- 2",
    'review::qcComment("third check")'
  ), tf2)
  
  result <- qcOutstanding(c(tf1, tf2))
  
  # Test structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("script_name", "qc_comment", "line_number"))
  
  # Test content
  expect_equal(nrow(result), 3)
  expect_equal(
    result$qc_comment,
    c("check this", "another check", "third check")
  )
  expect_equal(result$line_number, c(2, 4, 2))
  
  # Test with different formatting
  tf3 <- tempfile(fileext = ".R")
  writeLines(c(
    'review::qcComment("test")',
    'qcComment("test2")',
    'review::qcComment("test3")'
  ), tf3)
  
  result2 <- qcOutstanding(tf3)
  expect_equal(nrow(result2), 3)
  
  # Clean up
  unlink(c(tf1, tf2, tf3))
  
  # Test error conditions
  expect_error(
    qcOutstanding("nonexistent.R"),
    "Some specified files do not exist"
  )
  expect_error(
    qcOutstanding(42),
    "files must be a character vector"
  )
})
