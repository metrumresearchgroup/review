test_that("svnLog does not work outside of SVN", {
  expect_error(svnLog("script/data-assembly.R"))
})

with_demoRepo({
  file1 <- "script/data-assembly.R"
  file2 <- "script/pk/load-spec.R"
  
  logFile1 <- svnLog(file1)
  logFile2 <- svnLog(file2)
  
  test_that("svnLog includes all commits in dataframe format", {
    expect_true(all(c("author", "datetime", "rev", "msg") %in% names(logFile1)))
    expect_equal(nrow(logFile1), 2)
    expect_equal(nrow(logFile2), 2)
    
    expect_equal(as.numeric(logFile1$rev[1]), 5)
    expect_equal(as.numeric(logFile2$rev[1]), 4)
  })
})

