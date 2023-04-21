test_dir <- createRepo()
withr::local_dir(test_dir)

# Add path test
system("mkdir script")
system("mkdir data")
add_file("script/file.txt", "something")
add_file("data/filespec.txt", "something")
logCreate()

# Confirm users are able to provide a path with ../ or give an absolute path
test_that("relpath outputs script paths relative to the QC log location: for unresolved or absolute path [REV-REL-001]", {
  expect_identical("script/file.txt", pathFromLogRoot("data/../script/file.txt"))
  expect_identical("script/file.txt", relPath("data/../script/file.txt"))
  expect_identical("script/file.txt", pathFromLogRoot("/tmp/svn-testing/test/script/file.txt"))
})

logAssign("script/file.txt")
Sys.sleep(1)
logAssign("data/../script/file.txt")
Sys.sleep(1)
logAssign("script/../data/filespec.txt")
add_commit("first")
qclog <- readr::read_csv("QClog.csv") %>% suppressMessages()

test_that("relpath outputs script paths relative to the QC log location - only resolved file paths are stored in the QClog [REV-REL-001]", {
  expect_true(nrow(qclog) == 3)
  expect_identical(
    unique(qclog$file), 
    c("script/file.txt", "data/filespec.txt")
  )
})

# Add test for legacy logs with unresolved file paths
qclog_old <- qclog %>% dplyr::slice(1) %>% dplyr::mutate(file = "data/../script/file.txt",
                                                         origin = "data/../script/file.txt")
qclog_combine <- qclog %>% dplyr::bind_rows(qclog_old)
readr::write_csv(qclog_combine, "QClog.csv") 

# Confirm legacy QClogs will not show duplicate records for same file with different paths
test_that("logPending only includes resolved paths when unresolved paths present [REV-PND-002]", {
  pendingcsv <- logPending()
  expect_identical(
    pendingcsv$file, 
    c("data/filespec.txt", "script/file.txt")
  )
})

