with_demoRepo({
  if (Sys.getenv("METWORX_VERSION") != "") {
    test_that("compareModified doesn't return anything", {
      
      expect_message({
        compareModified(fs::path_abs("deliv/figure/example-pdf1.pdf"))
      })
      
      expect_message({
        compareModified("deliv/figure")
      })
      
      expect_message({
        compareModified("deliv/table")
      })
      
      
      expect_error({
        compareModified("deliv/table", .file_exts = c("png", "jpeg"))
      })
    })
  }
})
