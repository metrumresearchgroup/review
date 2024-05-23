with_demoRepo({
  if (Sys.getenv("METWORX_VERSION") != "") {
    test_that("compareTables works with standard case", {
      
      x <- compareTables(.path_current = "deliv/table")
      y <- compareTables(.path_current = "deliv/table/example-table-1.tex")
      z <- compareTables(.path_current = "deliv/table/example-table-long-1.tex", 
                          .path_previous = "deliv/table/example-table-1.tex")
      
      expect_true(
        any(grepl("example-table-1.tex", readLines(x))) &
          any(grepl("example-table-long-1.tex", readLines(x)))
      )
      
      expect_true(
        any(grepl("example-table-1.tex", readLines(y))) &
          !any(grepl("example-table-long-1.tex", readLines(y)))
      )
      
      expect_true(
        any(grepl("example-table-long-1.tex", readLines(z)))
      )
      
      expect_error({
        compareFigures(.path_current = "deliv/figure/example png1.png")
      })
      
    })
  }
})
