create_test_svn()

logAssign("script/model-management.R")
logAccept("script/box-sample-code.R")

dirSummaryRes <- dirSummary()
dirSummaryResExcl <- dirSummary(.dirs_exclude = "script")
  
# Check that QC summary contains expected information
test_that("dirSummary returns the correct project name", {
  expect_equal(basename(logRoot()), dirSummaryRes$project)
})
  
test_that("dirSummary returns all expected output", {
  expect_equal(c("project", "data", "status", "wd"), names(dirSummaryRes))
  expect_true(length(dirSummaryRes) == 4)
  expect_true(is.character(dirSummaryRes$project))
  expect_true(is.data.frame(dirSummaryRes$data))
  expect_true(is.data.frame(dirSummaryRes$status))
  expect_true(length(dirSummaryResExcl) == 4)
})
  
test_that("dirSummary captures the expected QC status of all scripts", {
  
  expect_equal(
    "script/box-sample-code.R",
    dirSummaryRes$data %>% dplyr::filter(Status == "QC up to date") %>% dplyr::pull(File)
  )
  
  # Latest rev
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/box-sample-code.R") %>% dplyr::pull(`Latest rev`) == "1")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/data-assembly/da-study-abc.Rmd") %>% dplyr::pull(`Latest rev`) == "7")

  # Status
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/box-sample-code.R") %>% dplyr::pull(Status) == "QC up to date")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/model-management.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/model-summary.Rmd") %>% dplyr::pull(Status) == "Not in QC log")
})
  
# QC Status data.frame
test_that("dirSummary generates formatted dataframe of QC status", {
  expect_true(is.factor(dirSummaryRes$status$Status))
  expect_true(nrow(dirSummaryRes$status %>% dplyr::filter(Status == "QC up to date")) == 1)
  expect_true(nrow(dirSummaryRes$status %>% dplyr::filter(Status == "In QC log, needs QC")) == 1)
  expect_true(nrow(dirSummaryRes$status %>% dplyr::filter(Status == "Not in QC log")) == 15)
})

setwd("script")
dirSummaryResDir <- dirSummary()

test_that("dirSummary works in a directory other than log root", {
  expect_identical(dirSummaryRes, dirSummaryResDir)
})
