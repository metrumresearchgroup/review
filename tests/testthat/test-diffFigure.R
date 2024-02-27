repo <- demoRepo("abc-123")
setwd(repo)

test_that("diffFigure works with demoRepo pdfs", {
  
  x <- diffFigure("example-pdf1.pdf", "example-pdf1.pdf")
  
  expect_true(length(x) == 4)
  
})
