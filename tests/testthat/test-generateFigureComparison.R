with_demoRepo({
  
  path1 <- "deliv/figure"
  path2 <- "deliv/figure/example-pdf1.pdf"
  path3 <- "deliv/figure/example-pdf4.pdf"
  
  test_that("generateFigureComparison returns the expected data.frame", {
    
    df1 <- generateFigureComparison(path1)
    df2 <- generateFigureComparison(path2)
    expect_error(generateFigureComparison(path3))
    
    expect_equal(nrow(df1), 4)
    expect_equal(nrow(df2), 1)
    
    expect_equal(names(df1), names(df2))
    
  })
})
