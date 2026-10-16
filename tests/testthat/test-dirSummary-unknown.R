test_that("dirSummary distinguishes unknown revisions from current and pending QC", {
  fixture <- test_path("fixtures", "dir-summary", "QClog.csv")
  work <- withr::local_tempdir()
  file.copy(fixture, file.path(work, "QClog.csv"))
  withr::local_dir(work)
  dir.create("script")
  log <- logRead()
  files <- c(log$file, "script/unassigned.R")
  file.create(c(files, "origin.txt"))
  tracked <- files[files != "script/not-svn.R"]
  testthat::local_mocked_bindings(
    revision = function(file) {
      if (basename(file) %in% c("absent.txt", "missing-head.R", "pending-origin-unknown.R")) {
        return(NA_real_)
      }
      10
    },
    svnList = function() data.frame(
      file = tracked, author = "test",
      datetime = as.POSIXct("2026-01-01", tz = "UTC"), rev = 10
    ),
    .package = "review"
  )

  expect_false(file.exists("absent.txt"))
  expect_true(all(is.na(log$revf[log$file %in% c("script/missing-revf.R", "script/missing-recorded.R")])))
  result <- dirSummary()
  expected <- c(
    "script/missing-origin.R" = "QC status unknown",
    "script/missing-head.R" = "QC status unknown",
    "script/missing-revf.R" = "QC status unknown",
    "script/missing-revo.R" = "QC status unknown",
    "script/missing-recorded.R" = "QC status unknown",
    "script/missing-reviewer.R" = "QC up to date",
    "script/current.R" = "QC up to date",
    "script/pending.R" = "In QC log, needs QC",
    "script/pending-file-unknown.R" = "In QC log, needs QC",
    "script/pending-origin-unknown.R" = "In QC log, needs QC",
    "script/pending-reviewer.R" = "In QC log, needs QC",
    "script/unassigned.R" = "Not in QC log"
  )
  actual <- setNames(as.character(result$data$Status), result$data$File)
  expect_setequal(names(actual), names(expected))
  expect_identical(actual[names(expected)], expected)
  expect_true(is.na(result$data$QCer[result$data$File == "script/missing-reviewer.R"]))

  for (table in list(result$data, result$status)) {
    expect_true(is.factor(table$Status))
    expect_false(anyNA(table$Status))
    expect_true("QC status unknown" %in% levels(table$Status))
    expect_equal(sum(table$Status == "QC status unknown"), 5)
  }
  counts <- c("QC status unknown" = 5, "QC up to date" = 2,
              "In QC log, needs QC" = 4, "Not in QC log" = 1)
  expect_equal(result$status$Author,
               paste0("test (N=", unname(counts[as.character(result$status$Status)]), ")"))
})
