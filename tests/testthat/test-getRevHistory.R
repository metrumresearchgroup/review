with_demoRepo({
  file_w_1 <- "script/combine-da.R"
  file_w_2 <- "script/pk/load-spec.R"
  file_w_no_qc <- "script/examp-txt.txt"
  
  df1 <- getRevHistory(file_w_1)
  df2 <- getRevHistory(file_w_2)
  df3 <- getRevHistory(file_w_no_qc)
  
  test_that("getRevHistory returns expected size dataframes", {
    expect_equal(nrow(df1), 2)
    expect_equal(nrow(df2), 3)
    expect_equal(nrow(df3), 2)
    
    expect_identical(names(df1), names(df2))
    expect_true(all(c("author", "elapsed", "rev", "msg", "QCed") %in% names(df1)))
  })
  
  test_that("getRevHistory works if file has not been QCed", {
    expect_identical(df2$QCed[1], "No")
    expect_identical(df2$QCed[2], "No")
    expect_identical(df2$QCed[3], "Yes")
    expect_identical(df2$msg[1], "Working copy")
    expect_identical(df2$msg[2], "modify load-spec script")
    
    expect_true(all(df3$QCed == "No"))
  })
  
  test_that("getRevHistory alerts the user a file is not checked into SVN", {
    expect_error(getRevHistory("script/combine-da2.R"), "'script/combine-da2.R' does not exist.")
  })
  
})
