create_test_svn()
logAccept("script/data-assembly/da-functions.R")
logAssign("script/model-summary.Rmd")

test_that("Function requires single file input", {
  da_files <- list.files("script/data-assembly", full.names = TRUE)
  expect_error(fileSummary(da_files), "must be a single file path")
})

test_that("Works correctly for a single file QC filed", {
  da_functions <- fileSummary("script/data-assembly/da-functions.R")
  
  expect_true(grepl("Yes", as.character(da_functions$qclog)))
  expect_true(grepl("0 days ago", as.character(da_functions$prevQC)))
  expect_true(grepl("QC up to date", as.character(da_functions$qcstatus)))
})

test_that("Output structure is correct for non QCed file", {
  da_study_abc <- fileSummary("script/data-assembly/da-study-abc.Rmd")
  
  expect_true(grepl("No", as.character(da_study_abc$qclog)))
  expect_true(grepl("No previous QC", as.character(da_study_abc$prevQC)))
  expect_true(grepl("Not assigned", as.character(da_study_abc$qcstatus)))
})

test_that("Function handles non-existent files appropriately", {
  expect_warning(
    non_nonexistent <- fileSummary("nonexistent-file.R"),
    "does not exist"
  )
  expect_null(non_nonexistent)
})
