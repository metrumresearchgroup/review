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

# renderQCSummary(.dir = logRoot(), .output_dir = logRoot())
# 
# test_that("renderQCSummary works with valid directory", {
#   # Check that the output file was created
#   expect_true(file.exists(file.path(logRoot(), paste0("qc-summary-", Sys.Date(), ".pdf"))))
#   
# })

temp_dir <- tempdir()
renderQCSummary(.dir = logRoot())

test_that("renderQCSummary doesn't save file if directory not given", {
  
  # Check that the output file was created
  expect_true(file.exists(file.path(temp_dir, paste0("qc-summary-", Sys.Date(), ".pdf"))))
  
})
