test_dir <- createRepo()
withr::local_dir(test_dir)

# Add path test
system("mkdir script")
system("mkdir data")
add_file("script/file.txt", "something")
add_file("data/filespec.txt", "something")
logCreate()

test_that("relpath outputs the relative path when given an unresolved or absolute path", {
  expect_identical("script/file.txt", pathFromLogRoot("data/../script/file.txt"))
  expect_identical("script/file.txt", relPath("data/../script/file.txt"))
  expect_identical("script/file.txt", pathFromLogRoot("/tmp/svn-testing/test/script/file.txt"))
})

logAssign("script/file.txt")
logAssign("data/../script/file.txt")
logAssign("script/../data/filespec.txt")
add_commit("first")
qclog <- readr::read_csv("QClog.csv")

test_that("Only resolved file paths are stored in the QClog", {
  expect_true(length(unique(qclog$file)) == 2)
})

# Add test for legacy logs with unresolved file paths
qclog_old <- qclog %>% dplyr::slice(1) %>% dplyr::mutate(file = "data/../script/file.txt",
                                                         origin = "data/../script/file.txt")
qclog_combine <- qclog %>% dplyr::bind_rows(qclog_old)
readr::write_csv(qclog_combine, "QClog.csv")

test_that("logPending only includes resolved paths when unresolved paths present", {
  pendingcsv <- logPending()
  expect_true(length(unique(pendingcsv$file)) == 2)
})

