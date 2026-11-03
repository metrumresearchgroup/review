with_demoRepo({
  pending <- logPending()
  
  test_that("logPending creates a row in the QClog for all non QCed files", {
    expect_true(nrow(pending) == 3)
    expect_true(all(pending$headf > pending$revf))
    expect_true("script/data-assembly.R" %in% pending$file)
    expect_true("script/examp-txt.txt" %in% pending$file)
  })

  test_that("logPending preserves file information when revisions are missing", {
    expected <- logSummary()

    svnRun("update", "--set-depth", "empty", "script")
    pending <- suppressWarnings(logPending())

    expect_equal(pending$file, expected$file)
    expect_equal(pending$reviewer, expected$reviewer)
    expect_equal(pending$revf, expected$revf)
    expect_equal(pending$revo, expected$revo)
  })
})

test_that("logPending includes unavailable and pending files but excludes up-to-date files", {
  with_demoRepo({
    svnRun("update", "--set-depth", "empty", "script/pk")
    summary <- suppressWarnings(logSummary())
    pending <- suppressWarnings(logPending())

    unavailable <- summary[summary$file == "script/pk/load-spec.R", ]
    available_pending <- summary[summary$file == "script/data-assembly.R", ]
    up_to_date <- summary[summary$file == "script/combine-da.R", ]

    expect_false(file.exists(unavailable$file))
    expect_true(is.na(unavailable$headf))
    expect_true(file.exists(available_pending$file))
    expect_gt(available_pending$headf, available_pending$revf)
    expect_true(file.exists(up_to_date$file))
    expect_equal(up_to_date$headf, up_to_date$revf)
    expect_equal(up_to_date$heado, up_to_date$revo)
    expect_setequal(pending$file, c(
      "script/pk/load-spec.R",
      "script/data-assembly.R",
      "script/examp-txt.txt"
    ))
  })
})

test_that("reassignment reopens QC even when entries share a timestamp", {
  with_demoRepo({
    file <- "script/examp-yaml.yaml"
    time <- "2099-01-01 00:00:00 GMT"
    for (round in 1:2) {
      logAssign(file, reviewer = "alice", time = time)
      pending <- logPending()
      expect_true(file %in% pending$file)
      expect_equal(pending$revf[pending$file == file], 0)
      expect_equal(pending$reviewer[pending$file == file], "alice")
      logAccept(file, reviewer = "alice", time = time)
      expect_false(file %in% logPending()$file)
    }
  })
})

test_that("later revisions can still be accepted without reassignment", {
  with_demoRepo({
    file <- "script/examp-yaml.yaml"
    logAssign(file)
    logAccept(file)
    expect_false(file %in% logPending()$file)
    cat("\n# Updated for another QC round\n", file = file, append = TRUE)
    svnRun("commit", "-m", "update file for QC")
    expect_true(file %in% logPending()$file)
    logAccept(file)
    expect_false(file %in% logPending()$file)
  })
})
