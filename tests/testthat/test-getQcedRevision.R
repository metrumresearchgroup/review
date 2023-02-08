test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")

logCreate()

add_commit("first")

logAssign(file = "file.txt")
logAccept(file = "file.txt")

test_that("getQcedRevision finds the latest accepted revision number [REV-GQR-001]", {
  expect_true(getQcedRevision("file.txt") == 1)
})

system("echo 'something else' > file.txt")
add_commit("second")

logAssign(file = "file.txt")
logAccept(file = "file.txt")

test_that("getQcedRevision finds the latest accepted revision number [REV-GQR-001]", {
  expect_true(getQcedRevision("file.txt") == 2)
})
