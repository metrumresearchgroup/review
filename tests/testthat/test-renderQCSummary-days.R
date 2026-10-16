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

test_that("renderQCSummary renders custom and unlimited lookbacks to PDF", {
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is required")
  skip_if(Sys.which("pdflatex") == "", "pdflatex is required")
  skip_if(Sys.which("kpsewhich") == "", "kpsewhich is required")
  caption <- suppressWarnings(system2("kpsewhich", "caption.sty", stdout = TRUE))
  skip_if(length(caption) == 0L, "The caption LaTeX package is required")

  work <- withr::local_tempdir()
  withr::local_dir(work)
  dir.create("script")
  files <- c("script/recent.R", sprintf("script/older-%03d.R", 1:100), "script/oldest.R")
  file.create(files)
  testthat::local_mocked_bindings(
    logRoot = function() work,
    svnProjInfo = function() list(url = "svn://example/project", rev = 102),
    repoHistory = function() data.frame(
      file = files, author = "test", rev = seq_along(files),
      date = Sys.Date() - c(5, rep(50, 100), 500)
    ),
    logRead = function() data.frame(file = character(), reviewer = character()),
    logPending = function() data.frame(file = character()),
    .package = "review"
  )

  for (days in c(60, Inf)) {
    output_dir <- file.path(tempdir(), "qc-summary-days", paste0("days-", days))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (is.infinite(days)) {
      expect_warning(renderQCSummary(output_dir, days = days), "very long report")
    } else {
      renderQCSummary(output_dir, days = days)
    }
    pdf <- file.path(output_dir, paste0("qc-summary-", Sys.Date(), ".pdf"))
    expect_true(file.exists(pdf))
    pages <- pdftools::pdf_text(pdf)
    content <- paste(pages, collapse = "\n")
    expect_true(grepl("recent.R", content, fixed = TRUE))
    expect_true(grepl("older-100.R", content, fixed = TRUE))
    expect_identical(grepl("oldest.R", content, fixed = TRUE), is.infinite(days))
    expect_gt(sum(grepl("older-", pages, fixed = TRUE)), 1L)
    message("PDF saved to: ", pdf)
  }
})
