with_demoRepo({
  if (Sys.getenv("METWORX_VERSION") != "") {
    test_that("compareFigures works with standard case", {
      
      x <- compareFigures(.path_current = "deliv/figure", .file_types = c("png", "pdf"))
      xx <- compareFigures(.path_current = "deliv/figure", .file_types = "png")
      y <- compareFigures(.path_current = "deliv/figure/example-pdf1.pdf", .file_types = c("png", "pdf"))
      z <- compareFigures(.path_current = "deliv/figure/example-pdf2.pdf",
                          .file_types = c("png", "pdf"),
                          .path_previous = "deliv/figure/example-pdf4.pdf")
      
      expect_true(
        any(grepl("example-pdf1.pdf", readLines(x))) &
          any(grepl("example-pdf2.pdf", readLines(x))) &
          any(grepl("example-pdf3.pdf", readLines(x)))
      )
      
      expect_true(
        any(grepl("example png1.png", readLines(xx))) &
          !any(grepl("example-pdf2.pdf", readLines(xx)))
      )
      
      expect_true(
        any(grepl("example-pdf1.pdf", readLines(y))) &
          !any(grepl("example-pdf2.pdf", readLines(y)))
      )
      
      expect_true(
        any(grepl("example-pdf2.pdf", readLines(z)))
      )
      
      expect_error({
        compareFigures(.path_current = "deliv/figure/example png1.png")
      })
      
      expect_error({
        compareLocal("deliv/figure", "deliv/figure", .file_exts = "doc")
      })
      
    })
  }
})
