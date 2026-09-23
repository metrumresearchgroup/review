with_demoRepo({
  tempdf <- logRead()
  tempdf_sum <- logSummary()
  
  test_that("logSummary only shows latest approved revision of a file", {
    expect_true(nrow(tempdf) > nrow(tempdf_sum))
    expect_true(tempdf_sum %>% dplyr::filter(file == "script/data-assembly.R") %>% dplyr::pull(headf) == 5)
    expect_true(nrow(tempdf_sum %>% dplyr::count(file) %>% dplyr::count(n, name = "num_files")) == 1)
  })
  
  test_that("logSummary prints as a data.frame", {
    expect_true(inherits(logSummary(), "data.frame"))
  })
  
  time_pattern <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}( GMT)?$"
  
  test_that("logSummary reduces time to nearest second", {
    expect_true(all(grepl(time_pattern, tempdf_sum$time)))
  })
  
  test_that("logSummary warns when file no longer exists", {
    svnRun("mv", "script/data-assembly.R", "script/data-assembly-new.R")
    svnRun("commit", "-m", "move script")

    # Expect two warnings because revision() is called on file column and origin
    # column.
    pat <- "svn info"
    expect_warning(expect_warning(logSummary(), pat), pat)
  })
})

