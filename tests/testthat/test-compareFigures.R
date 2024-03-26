with_demoRepo({
  if (Sys.getenv("METWORX_VERSION") != "") {
    test_that("compareFigures doesn't return anything", {
      
      expect_silent({
        compareFigures(fs::path_abs("deliv/figure/example-pdf1.pdf"))
      })
      
      expect_silent({
        compareFigures("deliv/figure")
      })
    })
  }
})
