test_dir <- createRepo()
withr::local_dir(test_dir)

logCreate()

test_that("logCreate generates a csv file [REV-CRT-001]", {
  expect_true(file.exists("QClog.csv"))
})

