test_that("renderQCSummary applies the lookback and supports all history", {
  work <- withr::local_tempdir()
  withr::local_dir(work)
  dir.create("script")
  files <- c("script/recent.R", "script/older.R")
  file.create(files)
  rendered <- ""

  testthat::local_mocked_bindings(
    logRoot = function() work,
    svnProjInfo = function() list(url = "svn://example/project", rev = 2),
    repoHistory = function() data.frame(
      file = files, author = "test", rev = 1:2,
      date = Sys.Date() - c(5, 50)
    ),
    logRead = function() data.frame(file = character(), reviewer = character()),
    logPending = function() data.frame(file = character()),
    .package = "review"
  )
  testthat::local_mocked_bindings(
    render = function(input, params, knit_root_dir, ...) {
      root <- knitr::opts_knit$get("root.dir")
      on.exit(knitr::opts_knit$set(root.dir = root))
      knitr::opts_knit$set(root.dir = knit_root_dir)
      env <- new.env()
      env$params <- params
      output <- knitr::knit(input, output = tempfile(), envir = env, quiet = TRUE)
      rendered <<- paste(readLines(output), collapse = "\n")
    },
    .package = "rmarkdown"
  )

  renderQCSummary()
  expect_true(grepl("recent.R", rendered, fixed = TRUE))
  expect_false(grepl("older.R", rendered, fixed = TRUE))
  renderQCSummary(days = 60)
  expect_true(grepl("older.R", rendered, fixed = TRUE))
  expect_warning(renderQCSummary(days = Inf), "very long report")
  expect_true(grepl("older.R", rendered, fixed = TRUE))

  for (days in list(-1, -Inf, NA_real_, NaN, "20", numeric(), c(10, 20))) {
    expect_error(renderQCSummary(days = days), "single non-negative number")
  }
})
