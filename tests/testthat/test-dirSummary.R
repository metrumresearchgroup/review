repo <- demoRepo("abc-123")
setwd(repo)
test_dir <- "script"
dirSummaryRes <- dirSummary(test_dir)

# Check that QC summary contains expected information
test_that("dirSummary returns the same directory and project provided to it", {
  expect_equal(test_dir, dirSummaryRes$directory)
  expect_equal(basename(logRoot()), dirSummaryRes$project)
})

test_that("dirSummary captures the expected QC status of all scripts", {
  
  expect_equal(
    "script/combine-da.R",
    dirSummaryRes$data %>% dplyr::filter(Status == "QC up to date") %>% dplyr::pull(File)
  )
  
  # Latest rev
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/combine-da.R") %>% dplyr::pull(`Latest rev`) == "1")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/data-assembly.R") %>% dplyr::pull(`Latest rev`) == "5")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/examp-yaml.yaml") %>% dplyr::pull(`Latest rev`) == "1")
  
  # Status
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/combine-da.R") %>% dplyr::pull(Status) == "QC up to date")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/data-assembly.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/examp-yaml.yaml") %>% dplyr::pull(Status) == "Not in QC log")
  expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/pk/load-spec.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
  
  # Date
  expect_equal(as.Date(dirSummaryRes$data$`Latest edit`[1]), Sys.Date())
  
  # Author
  expect_equal(dirSummaryRes$data$Author[1], Sys.info()[["user"]])
  
})

# Test renderQCSummary helpers

test_that("formatDirSummary generates formatted dataframes using the dirSummary output", {
  helper_output <- formatDirSummary(dirSummaryRes$data)
  expect_equal(length(helper_output), 2)
  expect_true(is.factor(helper_output$dirSummary$Status))
  expect_true(nrow(helper_output$dirSummary) == 4)
  expect_true(is.factor(helper_output$qcStatus$Status))
})
