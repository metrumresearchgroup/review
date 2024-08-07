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
  
})

