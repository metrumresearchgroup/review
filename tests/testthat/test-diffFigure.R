test_that("diffFigure works with demoRepo pdfs", {
  setwd(demoRepo("abc-123"))
  
  x <- diffFigure("example-pdf1.pdf", "example-pdf1.pdf")
  
  expect_true(length(x) == 4)
  
})
