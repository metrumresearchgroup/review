test_dir <- createRepo()
withr::local_dir(test_dir)

system("echo 'something' > file.txt")

logCreate()

system("svn add *")
system("svn commit -m 'first commit'") 

logAssign(file = "file.txt")
tempdf <- readr::read_csv("QClog.csv") %>% suppressMessages()

test_that("logAssign creates a row in the QClog for the specified file [REV-ASN-001]", {
  expect_true(tempdf$file[1] == "file.txt")
})
