# Tests for qcComment
test_that("qcComment handles input correctly", {
  expect_equal(
    qcComment("test comment"),
    "test comment"
  )
  expect_error(
    qcComment(c("multiple", "comments")),
    "comment must be a single character string"
  )
})
