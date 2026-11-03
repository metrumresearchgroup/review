with_demoRepo({
  test_that("requester is recorded and retained on acceptance", {
    file <- "script/examp-yaml.yaml"
    logAssign(file)
    expect_equal(tail(logRead()$requester, 1), unname(Sys.info()["user"]))
    logAssign(file, requester = "alice")
    logAccept(file, reviewer = "bob")
    expect_equal(tail(logRead()$requester, 1), "alice")
    expect_equal(tail(logRead()$reviewer, 1), "bob")
    summary <- logSummary()
    expect_equal(summary$requester[summary$file == file], "alice")
  })

  test_that("legacy logs can be read and extended", {
    old <- logRead()
    old$requester <- NULL
    logWrite(old, logName())
    expect_true(all(is.na(logRead()$requester)))
    logAssign("script/examp-yaml.yaml", requester = "carol")
    updated <- logRead()
    expect_true(all(is.na(head(updated$requester, -1))))
    expect_equal(tail(updated$requester, 1), "carol")
  })
})
