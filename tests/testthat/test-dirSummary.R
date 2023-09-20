test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.R", "something")
add_file("file2.R", "something")
add_file("file3.R", "something")
add_file("file4.R", "something")
add_file("file5.R", "something")

logCreate()
add_commit("first")
# Assign all scripts to QC log
logAssign(file = "file.R")
logAssign(file = "file2.R")
logAssign(file = "file3.R")
logAssign(file = "file4.R")
logAssign(file = "file5.R")

# Accept a few scripts
logAccept(file = "file.R")
logAccept(file = "file2.R")
logAccept(file = "file3.R")

# Make edit to one file
add_file("file.R", "something-new")
add_commit("second")

dirSummaryRes <- dirSummary(test_dir)

# Check that QC summary contains expected information
test_that("dirSummary returns the same directory and project provided to it", {
  expect_equal(test_dir, dirSummaryRes$directory)
  expect_equal(basename(logRoot()), dirSummaryRes$project)
})

test_that("dirSummary captures the expected QC status of all scripts", {
  
  expect_equal(
    c("file2.R", "file3.R"),
    dirSummaryRes$data %>% dplyr::filter(Status == "QC up to date") %>% dplyr::pull(File)
  )
  
  # Latest rev
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file.R") %>% dplyr::pull(`Latest rev`) == 2)
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file2.R") %>% dplyr::pull(`Latest rev`) == 1)
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file5.R") %>% dplyr::pull(`Latest rev`) == 1)
  
  # Status
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file2.R") %>% dplyr::pull(Status) == "QC up to date")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file3.R") %>% dplyr::pull(Status) == "QC up to date")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file4.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "file5.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
  
  # Date
  expect_equal(as.Date(dirSummaryRes$data$`Latest edit`[1]), Sys.Date())
  
  # Author
  expect_equal(dirSummaryRes$data$Author[1], Sys.info()[["user"]])
  
})
