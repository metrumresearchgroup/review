with_demoRepo({
  file1 <- "script/data-assembly.R"
  file2 <- "script/combine-da.R"
  
  diff_data <- diffFiles(.file_1 = file1, .file_2 = file2)
  
  test_that("diffFiles outputs diff between two different files", {
    expect_true(length(diff_data@target) == 10)
    expect_true(length(diff_data@current) == 6)
    expect_equal(diff_data@target[1], diff_data@current[1])
    expect_equal(diff_data@target[4], "derived <- list(sl = list(),tv = list())")
    expect_equal(diff_data@target[6], "derived$sl$dm <- dm_0")
  })

  test_that("diffFiles produces a self-contained styled HTML page", {
    diff_html <- paste(as.character(diff_data), collapse = "\n")

    expect_match(diff_html, "<html", ignore.case = TRUE)
    expect_match(diff_html, "<style", ignore.case = TRUE)
  })
})

