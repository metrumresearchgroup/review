test_that("svnInfo does not work outside of SVN", {
  expect_error(svnInfo("script/data-assembly.R"))
})

with_demoRepo({
  file1 <- "script/data-assembly.R"
  file2 <- "script/pk/load-spec.R"
  file3 <- "script/combine-da.R"
  file4 <- "script/examp-yaml.yaml"
  
  logFile1 <- svnInfo(file1)
  logFile2 <- svnInfo(file2)
  logFile3 <- svnInfo(file3)
  logFile4 <- svnInfo(file4)
  
  test_that("svnLog includes all commits in dataframe format", {
    expect_true(all(c("author", "datetime", "rev") %in% names(logFile1)))
    expect_equal(nrow(logFile1), 1)
    expect_equal(nrow(logFile2), 1)
    
    expect_equal(as.numeric(logFile1$rev), 5)
    expect_equal(as.numeric(logFile2$rev), 4)
    expect_equal(as.numeric(logFile3$rev), 1)
    expect_equal(as.numeric(logFile4$rev), 1)
  })
})


