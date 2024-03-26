with_demoRepo({
  if (Sys.getenv("METWORX_VERSION") != "") {
    test_that("diffFigures doesn't return anything", {
      
      expect_silent({
        diffFigures(fs::path_abs("example-pdf1.pdf"))
      })
    })
    
    diff_data1 <- diffFigures(fs::path_abs("example-pdf1.pdf"), fs::path_abs("example-pdf2.pdf"))
    diff_data2 <- diffFigures(fs::path_abs("example-pdf1.pdf"))
    
    test_that("diffFigures returns the correct data", {
      
      expect_true(basename(diff_data1$path1) == "example-pdf1.pdf")
      expect_true(basename(diff_data1$path2) == "example-pdf2.pdf")
      
      expect_true(basename(diff_data2$path1) == "example-pdf1.pdf")
      expect_true(basename(diff_data2$path2) == "example-pdf1-6.pdf")
      expect_true(diff_data2$compname == "example-pdf1.pdf: repo vs local")
      
    })
    
  }
})
