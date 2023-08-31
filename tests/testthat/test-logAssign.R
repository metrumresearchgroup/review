test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")

logCreate()

add_commit("first")

logAssign(file = "file.txt")
tempdf <- readr::read_csv("QClog.csv") %>% suppressMessages()

test_that("logAssign creates a row in the QClog for the specified file", {
  expect_true(tempdf$file[1] == "file.txt")
})



