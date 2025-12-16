file <- system.file("review-tabs-figs.pdf", package = "review")

df <- getTFs(.file = file)

testthat::test_that("getTFs returns a data.frame with one value per distinct file", {
  
  expect_true(nrow(df %>% dplyr::distinct(path, code_path)) == nrow(df))
  
})

test_that("getTFs parses tables/figures and pairs them with the correct source code", {

  # Adjust these to match where you store the example PDF in your package
  pdf <- system.file("review-tabs-figs.pdf", package = "review")
  expect_true(file.exists(pdf))
  
  res <- getTFs(pdf)
  
  # shape / columns
  expect_s3_class(res, "data.frame")
  expect_true(all(c("path", "code_path") %in% names(res)))
  expect_false("idx" %in% names(res))
  
  # should only include rows with a path (tables + figures), not "Source code" rows
  expect_false(any(is.na(res$path)))
  expect_false(any(is.na(res$code_path)))
  
  # this example PDF has 7 Source file entries + 4 Source graphic entries = 11
  expect_equal(nrow(res), 11)
  
  # ordering + correct pairing (first chunk uses eda-tables-1.R)
  expect_equal(res$code_path[[1]], "script/pk/eda-tables-1.R")
  expect_equal(res$path[[1]], "deliv/table/eda/id-sum-example-1.tex")
  
  # last row is the final figure, paired with pk-eda-figures-2.R
  expect_equal(res$code_path[[nrow(res)]], "script/pk/pk-eda-figures-2.R")
  expect_equal(res$path[[nrow(res)]], "deliv/figure/wt-mpg-scatter-2.pdf")
  
  # spot-check a figure row that is known to have unicode minus in the PDF text
  expect_true(any(res$code_path == "script/pk/pk-eda-figures.R"))
  expect_true(any(res$path == "deliv/figure/cont-vs-cont.pdf"))
})
