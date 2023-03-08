test_dir <- createRepo()
withr::local_dir(test_dir)

# Add path test
system("mkdir script")
system("mkdir data")
add_file("script/file.txt", "soemthing")
add_file("data/filespec.txt", "soemthing")

add_file("file.txt", "something")

logCreate()
add_commit("first")
logAssign(file = "file.txt")

# Add scripts from their own directory and different directory
setwd("/tmp/svn-testing/test/script")
logAssign(file = "file.txt")
setwd("/tmp/svn-testing/test/data")
logAssign(file = "../script/file.txt")

logAssign(file = "filespec.txt")
setwd("/tmp/svn-testing/test/script")
logAssign(file = "../data/filespec.txt")

logAccept(file = "file.txt")
tempdf <- readr::read_csv("QClog.csv") %>% suppressMessages()

test_that("logAccept creates a row in the QClog for the specified file [REV-ACC-001]", {
  expect_true(tempdf$reviewer[2] == Sys.info()[["effective_user"]])
  expect_true(tempdf$revf[2] == 1)
})



expect_identical("script/file.txt", clean_path("data/../script/file.txt"))
expect_identical("script/file.txt", clean_path("/tmp/svn-testing/test/script/file.txt"))


path_from_log_root <- function(.path){
  
  cur_dir <- getwd()
  on.exit(setwd(cur_dir))
  
  log_root <- review:::logRoot()
  setwd(log_root)
  
  full_path <- as.character(fs::path_abs(.path))
  full_here <- as.character(fs::path_abs(log_root))
  
  .ans <- gsub(paste0(full_here, "/"), "", full_path, fixed = TRUE)
  
  .ans
}
