test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")
add_file("file2.txt", "something")
add_file("file3.txt", "something")
add_file("file4.txt", "something")
add_file("file5.txt", "something")
add_file("file6.txt", "something")

logCreate()

add_commit("first")

logAssign(file = "file.txt")
logAssign(file = "file2.txt")
logAssign(file = "file3.txt")

logAccept("file.txt")
logAccept("file2.txt")
logAccept("file3.txt")

add_file("file.txt", "something2")

add_commit("second")

tempdf <- checkDirQC(.dir_path = getwd())

test_that("checkDirQC correctly assigns categories for files in and not in QC log [REV-CQC-001]", {
  expect_equal(nrow(tempdf %>% dplyr::filter(QCLog == "Y")), 3)
  expect_equal(nrow(tempdf %>% dplyr::filter(QCLog == "N")), 4)
  expect_equal(nrow(tempdf %>% dplyr::filter(QCPending == "Y")), 1)
  
  expect_true(tempdf %>% dplyr::filter(QCPending == "Y") %>% dplyr::pull(file) == "file.txt")
  expect_true(tempdf$QCLog[tempdf$file == "file4.txt"] == "N")
  expect_true(tempdf$QCLog[tempdf$file == "file2.txt"] == "Y")
  expect_true(tempdf$QCLog[tempdf$file == "file3.txt"] == "Y")
  expect_true(tempdf$QCLog[tempdf$file == "file.txt"] == "Y")
  
  expect_true(tempdf$QCPending[tempdf$file == "file.txt"] == "Y")
  expect_true(tempdf$QCPending[tempdf$file == "file2.txt"] == "N")
  expect_true(tempdf$QCPending[tempdf$file == "file4.txt"] == "N")
})
