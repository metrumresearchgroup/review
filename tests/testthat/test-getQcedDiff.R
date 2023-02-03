test_dir <- createRepo()
withr::local_dir(test_dir)

system("echo 'something' > file.txt")

logCreate()

system("svn add *")
system("svn commit -m 'first commit'") 

logAssign(file = "file.txt")
logAccept(file = "file.txt")

diffqc <- getQcedDiff("file.txt")

test_that("getQcedDiff reports no difference between identical files [REV-GQD-001]", {
  expect_true(diffqc@target == diffqc@current)
})

system("echo 'something else' > file.txt")
diffqc <- getQcedDiff("file.txt")

test_that("getQcedDiff identifies difference between local and QCed file version [REV-GQD-002]", {
  expect_true(diffqc@target != diffqc@current)
})

