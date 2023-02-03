test_dir <- createRepo()
withr::local_dir(test_dir)

system("echo 'something' > file.txt")

logCreate()
system("svn add *")
system("svn commit -m 'first commit'")
logAssign(file = "file.txt")

logAccept(file = "file.txt")
tempdf <- readr::read_csv("QClog.csv") %>% suppressMessages()

test_that("logAccept creates a row in the QClog for the specified file [REV-ACC-001]", {
  expect_true(tempdf$reviewer[2] == Sys.info()[["effective_user"]])
  expect_true(tempdf$revf[2] == 1)
})