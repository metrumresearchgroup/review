repo <- demoRepo("abc-123")
setwd(repo)

orig_qc <- readr::read_csv("QClog.csv") %>% suppressMessages()
file1 <- "script/examp-yaml.yaml"

test_that("logAssign creates a row in the QClog for the specified file", {
  logAssign(file1)
  new_qc <- readr::read_csv("QClog.csv") %>% suppressMessages()
  expect_equal(nrow(new_qc), nrow(orig_qc) + 1)
  expect_true(new_qc %>% dplyr::filter(file == file1) %>% dplyr::pull(revf) == 0)
})

