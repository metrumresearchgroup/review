test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")

logCreate()

add_commit("first")

logAssign(file = "file.txt")
logAccept(file = "file.txt")

diffqc <- diffQced("file.txt") %>% suppressMessages()

test_that("diffQced reports no difference between identical files", {
  expect_true(is.null(diffqc))
})

add_file("file.txt", "something else")
test_that("diffQced throws a warning if the file hasn't been updated with svn up", {
  expect_error(diffqc <- diffQced("file.txt"))
})

add_commit("second")
diffqc <- diffQced("file.txt")

test_that("diffQced identifies difference between local and QCed file version", {
  expect_true(diffqc@target != diffqc@current)
})

