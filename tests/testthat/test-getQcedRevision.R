test_dir <- createRepo()
withr::local_dir(test_dir)

system("mkdir script")
system("echo 'something' > script/file.txt")

logCreate()

system("svn add *")
system("svn commit -m 'first commit'") 

logAssign(file = "script/file.txt")
logAccept(file = "script/file.txt")

test_that("getQcedRevision finds the latest accepted revision number [REV-GQR-001]", {
  expect_true(getQcedRevision("script/file.txt") == 1)
})

system("echo 'something else' > script/file.txt")
system("svn add script/file.txt")
system("svn commit -m 'first commit'") 

logAssign(file = "script/file.txt")
logAccept(file = "script/file.txt")

test_that("getQcedRevision finds the latest accepted revision number [REV-GQR-001]", {
  expect_true(getQcedRevision("script/file.txt") == 2)
})
