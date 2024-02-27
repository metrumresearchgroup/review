with_demoRepo({
  file1 <- "script/data-assembly.R"
  
  logList <- svnCommand(.file = file1, .command = "log")
  infoList <- svnCommand(.file = file1, .command = "info")
  
  test_that("svnCommand has a list output and expected size of entries", {
    expect_equal(length(logList), 2)
    expect_equal(length(infoList), 1)
    expect_true(all(c("author", "date", ".attrs") %in% names(logList$logentry)))
    expect_true(all(c("wc-info", "commit", ".attrs") %in% names(infoList$entry)))
    expect_true(all(c("author", "date", ".attrs") %in% names(infoList$entry$commit)))
  })
  
  test_that("svnCommand identifies 2 changes to the file in the log output", {
    expect_true(as.numeric(logList[2]$logentry$.attrs) == 1)
    expect_true(as.numeric(logList[1]$logentry$.attrs) == 5)
  })
  
  test_that("svnCommand identifies most recent revision in svn info", {
    expect_true(infoList$entry$.attrs[["revision"]] == "5")
  })
})

