test_that("diffLocal compares local edits against BASE rather than repository HEAD", {
  with_demoRepo({
    file <- "script/pk/load-spec.R"
    svnRun("update", "-r", "1", file)
    base <- readLines(file)
    local <- c(base, "# uncommitted change")
    writeLines(local, file)

    diff <- diffLocal(file)
    expect_equal(diff@target, base)
    expect_equal(diff@current, local)

    writeLines(base, file)
    expect_message(expect_invisible(diffLocal(file)), "Specified files are identical")
  })
})
