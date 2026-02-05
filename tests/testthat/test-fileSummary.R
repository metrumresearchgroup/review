create_test_svn()
logAccept("script/data-assembly/da-functions.R")
logAssign("script/model-summary.Rmd")

test_that("Function requires single file or directory input", {
  da_files <- c(
    "script/data-assembly/da-functions.R",
    "script/data-assembly/da-study-abc.Rmd"
  )
  expect_error(fileSummary(da_files), "must be a single file or directory path")
})

test_that("Works correctly for a directory input", {
  da_directory <- fileSummary("script/data-assembly")
  da_files <- list.files("script/data-assembly", full.names = TRUE)
  da_files <- da_files[!fs::is_dir(da_files)]

  expect_true(is.list(da_directory))
  expect_setequal(names(da_directory), da_files)
  expect_true(all(vapply(da_directory, is.list, logical(1))))
})

test_that("Directory input matches purrr::walk-based file summaries", {
  testthat::skip_if_not_installed("purrr")

  da_files <- list.files("script/data-assembly", full.names = TRUE)
  da_files <- da_files[!fs::is_dir(da_files)]

  from_directory <- fileSummary("script/data-assembly")

  from_walk <- list()
  purrr::walk(
    da_files,
    ~ {
      from_walk[[.x]] <<- fileSummary(.file = .x)
    }
  )

  from_directory <- from_directory[order(names(from_directory))]
  from_walk <- from_walk[order(names(from_walk))]

  expect_equal(from_directory, from_walk)
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
