test_that("QC status consumers agree across reassignment and acceptance", {
  work <- withr::local_tempdir()
  withr::local_dir(work)
  dir.create("script")
  file <- "script/example.R"
  file.create(file)
  history <- data.frame(file = file, rev = 5L, author = "author", date = Sys.Date())
  testthat::local_mocked_bindings(
    revision = function(...) 5,
    svnXML = function(...) list(),
    svnLog = function(...) transform(history, datetime = as.POSIXct(Sys.Date())),
    repoHistory = function() history,
    svnProjInfo = function() list(this_svn_user = "author"),
    svnList = function() transform(history, datetime = as.POSIXct(Sys.Date())),
    .package = "review"
  )
  logCreate()
  for (reviewer in c("alice", "bob")) {
    logAssign(file, reviewer = reviewer)
    expect_true(file %in% logPending()$file)
    expect_equal(cli::ansi_strip(fileSummary(file)$qcstatus), "Needs QC")
    expect_true(file %in% review:::myQC()$`Awaiting QC`$File)
    expect_equal(as.character(dirSummary()$data$Status), "In QC log, needs QC")
    logAccept(file, reviewer = reviewer)
    expect_false(file %in% logPending()$file)
    expect_equal(cli::ansi_strip(fileSummary(file)$qcstatus), "QC up to date")
    expect_length(review:::myQC(), 0)
    expect_equal(as.character(dirSummary()$data$Status), "QC up to date")
  }
  logAssign(file, reviewer = "carol")
  expect_equal(getQcedRevision(file), 5)
  expect_true(all(c("alice", "bob") %in% logRead()$reviewer))
})

test_that("multiple QC reviewers do not duplicate repository history", {
  history <- data.frame(file = "script/example.R", rev = 5:4,
                        author = c("bob", "author"))
  log <- data.frame(file = "script/example.R", revf = c(5, 5, 0),
                    reviewer = c("alice", "bob", "carol"))
  result <- findAuthorsQcers(history, log)
  expect_equal(nrow(result$authors) + nrow(result$qcers), nrow(history))
  expect_equal(result$qcers$author, "bob")
  expect_equal(result$authors$author, "author")
})

test_that("reports distinguish pending assignments from completed QC", {
  work <- withr::local_tempdir()
  file <- "script/example.R"
  tables <- list()
  original_st_new <- pmtables::st_new
  testthat::local_mocked_bindings(
    st_new = function(x, ...) {
      tables[[length(tables) + 1L]] <<- x
      original_st_new(x, ...)
    },
    .package = "pmtables"
  )
  params <- list(
    project = "test", days = 20, projInfo = list(rev = 5),
    repoHistory = data.frame(file = file, rev = 5, author = "author", date = Sys.Date()),
    qcLog = data.frame(file = file, revf = c(5, 0), reviewer = c("alice", "bob")),
    logPending = data.frame(file = file),
    logSum = data.frame(file = file, revf = 0, headf = 5, time = "2026-01-01")
  )
  knitr::knit(system.file("templates", "QCSummary.Rmd", package = "review"),
              output = file.path(work, "summary.md"), envir = environment(), quiet = TRUE)
  expect_equal(tables[[1]]$`Previous QCers`, "alice")
  tables <- list()
  knitr::knit(system.file("templates", "QCReport.Rmd", package = "review"),
              output = file.path(work, "report.md"), envir = environment(), quiet = TRUE)
  expect_equal(tables[[1]]$Status, "Needs QC")
  expect_true(is.na(tables[[1]]$`QCed...revision`))
  expect_true(is.na(tables[[1]]$`QC date`))
})
