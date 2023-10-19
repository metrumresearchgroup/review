repo <- demoRepo("abc-123")
setwd(repo)

pending <- logPending()

test_that("logPending creates a row in the QClog for all non QCed files", {
  expect_true(nrow(pending) == 3)
  expect_true(all(pending$headf > pending$revf))
  expect_true("script/data-assembly.R" %in% pending$file)
  expect_true("script/examp-txt.txt" %in% pending$file)
})
