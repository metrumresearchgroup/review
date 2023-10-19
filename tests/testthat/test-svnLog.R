repo <- demoRepo("abc-123")

file1 <- file.path(repo, "script/data-assembly.R")
file2 <- file.path(repo, "script/pk/load-spec.R")

logFile1 <- svnLog(file1)
logFile2 <- svnLog(file2)

test_that("svnLog includes all commits in dataframe format", {
  expect_true(all(c("author", "datetime", "rev", "msg") %in% names(logFile1)))
  expect_equal(nrow(logFile1), 2)
  expect_equal(nrow(logFile2), 2)
  
  expect_equal(as.numeric(logFile1$rev[1]), 5)
  expect_equal(as.numeric(logFile2$rev[1]), 4)
})
