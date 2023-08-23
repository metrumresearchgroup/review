test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")
add_commit("first")

add_file("file.txt", "something2")
add_commit("second")

add_file("file.txt", "something3")
add_commit("third")

add_file("file.txt", "something4")
add_commit("fourth")

add_file("file.txt", "something5")
add_commit("fifth")

logList <- svnCommand(.file = "file.txt", .command = "log", .flags = c("v"))
infoList <- svnCommand(.file = "file.txt", .command = "info")

test_that("svnCommand has a list output and expected size of entries", {
  expect_equal(length(logList), 5)
  expect_equal(length(infoList), 1)
  expect_true(all(c("author", "date", ".attrs") %in% names(logList$logentry)))
  expect_true(all(c("wc-info", "commit", ".attrs") %in% names(infoList$entry)))
  expect_true(all(c("author", "date", ".attrs") %in% names(infoList$entry$commit)))
})

test_that("svnCommand identifies 5 changes to the file in the log output", {
  expect_true(as.numeric(logList[5]$logentry$.attrs) == 1)
  expect_true(as.numeric(logList[4]$logentry$.attrs) == 2)
  expect_true(as.numeric(logList[3]$logentry$.attrs) == 3)
  expect_true(as.numeric(logList[2]$logentry$.attrs) == 4)
  expect_true(as.numeric(logList[1]$logentry$.attrs) == 5)
})

test_that("svnCommand identifies most recent revision in svn info", {
  expect_true(infoList$entry$.attrs[["revision"]] == "5")
})
