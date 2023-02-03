test_dir <- createRepo()
withr::local_dir(test_dir)

system("echo 'something' > file.txt")
system("echo 'something' > file2.txt")
system("echo 'something' > file3.txt")

logCreate()

system("svn add *")
system("svn commit -m 'first commit'") 

logAssign(file = "file.txt")
logAssign(file = "file2.txt")
logAssign(file = "file3.txt")
tempdf <- logPending()

logAccept("file.txt")

pending <- logPending()


test_that("logPending creates a row in the QClog for all non QCed files [REV-PND-001]", {
  expect_true(nrow(tempdf) == 3)
  expect_true(nrow(pending) == 2)
  expect_true(pending$headf[1] == 1)
})
