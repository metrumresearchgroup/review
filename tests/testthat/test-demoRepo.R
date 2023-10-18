proj_name <- "abc-123"
repo <- demoRepo(proj_name)

test_that("demoRepo creates a temp directory with all expected files", {
  expect_true(dir.exists(repo))
  expect_equal(basename(repo), proj_name)
  expect_true(dir.exists(file.path(repo, "script/pk")))
  expect_true(file.exists(file.path(repo, "script", "data-assembly.R")))
  expect_true(file.exists(file.path(repo, "QClog.csv")))
})

qclog <- readr::read_csv(file.path(repo, "QClog.csv")) %>% suppressMessages()

test_that("demoRepo creates QC log and completes partial QC", {
  expect_true(
    qclog %>% 
      dplyr::count(revf == 0) %>% 
      dplyr::filter(`revf == 0`) %>% 
      dplyr::pull(n) == 4)
  
  expect_true(
    nrow(qclog %>% 
      dplyr::filter(file == "script/data-assembly.R")) == 2)
})
