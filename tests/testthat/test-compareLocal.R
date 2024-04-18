with_demoRepo({
  if (Sys.getenv("METWORX_VERSION") != "") {
    test_that("compareLocal doesn't return anything", {
      
      expect_message({
        compareLocal(fs::path_abs("deliv/figure/example-pdf1.pdf"),
                     fs::path_abs("deliv/figure/example-pdf1.pdf"))
      })
      
      expect_message({
        compareLocal("deliv/figure", "deliv/figure")
      })
      
      expect_message({
        compareLocal(.path = "deliv/table", .path_compare = "deliv/table")
      })
      
      expect_error({
        compareLocal("deliv/table", "deliv/figure")
      })
      
    })
  }
})
