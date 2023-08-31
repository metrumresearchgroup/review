test_dir <- createRepo()
withr::local_dir(test_dir)

logCreate()

test_that("logCreate generates a csv file", {
  expect_true(file.exists("QClog.csv"))
})

