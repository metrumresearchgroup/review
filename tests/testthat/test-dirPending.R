test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.R", "something")
add_file("file2.R", "something")
add_file("file3.R", "something")
add_file("file4.R", "something")
add_file("file5.R", "something")
add_file("file6.R", "something")

logCreate()

add_commit("first")

logAssign(file = "file.R")
logAssign(file = "file2.R")
logAssign(file = "file3.R")

logAccept("file.R")
logAccept("file2.R")
logAccept("file3.R")

add_file("file.R", "something2")

add_commit("second")

tempdf <- dirPending(.dir = getwd())

test_that("dirPending correctly assigns categories for files in and not in QC log", {
  expect_equal(nrow(tempdf %>% dplyr::filter(QCLog == "Y")), 3)
  expect_equal(nrow(tempdf %>% dplyr::filter(QCLog == "N")), 3)
  expect_equal(nrow(tempdf %>% dplyr::filter(QCed == "N")), 4)
  
  expect_true(tempdf %>% dplyr::filter(QCed == "N") %>% dplyr::slice(1) %>% dplyr::pull(file) == "file.R")
  expect_true(tempdf$QCLog[tempdf$file == "file4.R"] == "N")
  expect_true(tempdf$QCLog[tempdf$file == "file2.R"] == "Y")
  expect_true(tempdf$QCLog[tempdf$file == "file3.R"] == "Y")
  expect_true(tempdf$QCLog[tempdf$file == "file.R"] == "Y")
  
  expect_true(tempdf$QCed[tempdf$file == "file.R"] == "N")
  expect_true(tempdf$QCed[tempdf$file == "file2.R"] == "Y")
  expect_true(tempdf$QCed[tempdf$file == "file4.R"] == "N")
  
  expect_true(!any(grepl(".csv", tempdf$file)))
})
