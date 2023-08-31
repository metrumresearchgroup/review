test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")

logCreate()
add_commit("first")
logAssign(file = "file.txt")

logAccept(file = "file.txt")
tempdf <- readr::read_csv("QClog.csv") %>% suppressMessages()

tempdf_sum <- logSummary()

test_that("logSummary only shows latest approved revision of a file", {
  expect_true(nrow(tempdf) > nrow(tempdf_sum))
  expect_true(tempdf_sum$revf == 1)
})