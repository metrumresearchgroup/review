test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")
add_file("file2.txt", "something")
add_file("file3.txt", "something")

logCreate()

add_commit("first")

logAssign(file = "file.txt")
logAssign(file = "file2.txt")
logAssign(file = "file3.txt")
tempdf <- logPending()

logAccept("file.txt")

pending <- logPending()


test_that("logPending creates a row in the QClog for all non QCed files", {
  expect_true(nrow(tempdf) == 3)
  expect_true(nrow(pending) == 2)
  expect_true(pending$headf[1] == 1)
})
