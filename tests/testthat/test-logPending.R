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
