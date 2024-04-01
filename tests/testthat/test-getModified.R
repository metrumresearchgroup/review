with_demoRepo({
  
  path1 <- "deliv/figure"
  path2 <- "deliv/figure/example-pdf1.pdf"
  path3 <- "deliv/figure/example-pdf4.pdf"
  path4 <- "deliv/table"
  
  test_that("getModified returns the expected outputs", {
    
    df1 <- getModified(path1, c("png", "pdf"))
    expect_equal(nrow(df1), 4)
    expect_error(getModified(path1, c("tex")))
    
    
    df2 <- getModified(path2, c("pdf"))
    expect_equal(nrow(df2), 1)
    
    
    expect_error(getModified(path3, c("pdf")))
    
    
    df4 <- getModified(path4, c("tex"))
    expect_equal(nrow(df4), 2)
    
    
    expect_equal(names(df1), names(df2))
    expect_equal(names(df1), names(df4))
    
  })
})
