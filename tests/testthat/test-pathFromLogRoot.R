test_that("pathFromLogRoot resolves symbolic links before removing the log root", {
  fixture <- withr::local_tempdir()
  root <- file.path(fixture, "project")
  alias <- file.path(fixture, "alias")
  dir.create(root)
  dir.create(file.path(root, "script"))
  file.create(file.path(root, "QClog.csv"))
  file.create(file.path(root, "script", "data-assembly.R"))
  skip_if_not(file.symlink(root, alias), "Symbolic links are required")
  withr::local_dir(root)

  expect_identical(
    pathFromLogRoot(file.path(alias, "script", "data-assembly.R")),
    "script/data-assembly.R"
  )
  expect_identical(
    pathFromLogRoot(c("script/data-assembly.R", "script/missing.R")),
    c("script/data-assembly.R", "script/missing.R")
  )
  expect_identical(getwd(), normalizePath(root, winslash = "/"))
})
