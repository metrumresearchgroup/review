repo <- demoRepo("abc-123")

file1 <- file.path(repo, "script/data-assembly.R")
file2 <- file.path(repo, "script/combine-da.R")

diff_data <- diffFiles(.file_1 = file1, .file_2 = file2)

test_that("diffFiles outputs diff between two different files", {
  expect_true(length(diff_data@target) == 10)
  expect_true(length(diff_data@current) == 6)
  expect_equal(diff_data@target[1], diff_data@current[1])
  expect_equal(diff_data@target[4], "derived <- list(sl = list(),tv = list())")
  expect_equal(diff_data@target[6], "derived$sl$dm <- dm_0")
})
