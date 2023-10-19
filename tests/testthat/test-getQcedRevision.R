repo <- demoRepo("abc-123")

file1 <- "script/data-assembly.R"
file2 <- "script/combine-da.R"
file3 <- "script/examp-yaml.yaml"

setwd(repo)

test_that("getQcedRevision finds the latest accepted revision number", {
  expect_true(getQcedRevision(file1) == 1)
  expect_true(getQcedRevision(file2) == 1)
  expect_true(is.na(getQcedRevision(file3)))
})

