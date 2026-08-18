with_demoRepo({
  test_that("logRename renames the SVN file and all QC log references", {
    old <- "script/data-assembly.R"
    new <- "script/data assembly renamed.R"

    logAssign("script/examp-txt.txt", origin = old)
    before <- logRead()

    changed <- logRename(old, new)
    after <- logRead()
    svn_status <- system2("svn", c("status", "script"), stdout = TRUE)

    expect_false(file.exists(old))
    expect_true(file.exists(new))
    expect_false(old %in% after$file)
    expect_false(old %in% after$origin)
    expect_equal(sum(after$file %in% new | after$origin %in% new), changed)
    expect_equal(nrow(after), nrow(before))
    expect_true(any(grepl("data assembly renamed[.]R", svn_status)))
    expect_true(any(grepl("data-assembly[.]R", svn_status)))
  })
})

with_demoRepo({
  test_that("logRename validates paths before changing the working copy", {
    expect_error(
      logRename("script/not-in-log.R", "script/new-name.R"),
      "does not exist in QClog.csv",
      fixed = TRUE
    )
    expect_error(
      logRename("script/data-assembly.R", "script/combine-da.R"),
      "already exists",
      fixed = TRUE
    )
    expect_error(
      logRename("script/data-assembly.R", "missing/new-name.R"),
      "parent directory does not exist",
      fixed = TRUE
    )
    expect_true(file.exists("script/data-assembly.R"))
  })
})
