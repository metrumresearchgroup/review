test_dir <- createRepo()
withr::local_dir(test_dir)

system("echo 'something' > file.txt")

logCreate()

system("svn add *")
system("svn commit -m 'first commit'") 
logAssign(file = "file.txt")
logAccept(file = "file.txt")

system("echo 'something2' > file.txt")
system("svn commit -m 'second commit'") 
logAccept(file = "file.txt")

system("echo 'something3' > file.txt")
system("svn commit -m 'third commit'") 
logAccept(file = "file.txt")

system("echo 'something4' > file.txt")
system("svn commit -m 'fourth commit'") 
logAccept(file = "file.txt")

system("echo 'something5' > file.txt")

test_that("getPreviousRevisionDiff outputs diff between two previous specified versions [REV-GQD-001]", {
  diffVer <- getPreviousRevisionDiff(.file = "file.txt", .previous_revision = 2, .current_revision = 4)
  expect_true(diffVer@target != diffVer@current)
  expect_equal(diffVer@target, "something2")
  expect_equal(diffVer@current, "something4")
})

test_that("getPreviousRevisionDiff defaults current version of diff to local version [REV-GQD-002]", {
  diffVer <- getPreviousRevisionDiff(.file = "file.txt", .previous_revision = 2)
  expect_true(diffVer@target != diffVer@current)
  expect_equal(diffVer@target, "something2")
  expect_equal(diffVer@current, "something5")
  
  diffVer <- getPreviousRevisionDiff(.file = "file.txt", .previous_revision = 1)
  expect_true(diffVer@target != diffVer@current)
  expect_equal(diffVer@target, "something")
})
