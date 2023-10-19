repo <- demoRepo("abc-123")
setwd(repo)

# Confirm users are able to provide a path with ../ or give an absolute path
test_that("relpath outputs script paths relative to the QC log location: for unresolved or absolute path", {
  expect_identical("script/data-assembly.R", pathFromLogRoot(file.path(repo, "script/data-assembly.R")))
  expect_identical("script/data-assembly.R", relPath("script/pk/../data-assembly.R"))
})

logAssign("script/pk/../examp-yaml.yaml")
qclog <- readr::read_csv("QClog.csv") %>% suppressMessages()

test_that("relpath outputs script paths relative to the QC log location - only resolved file paths are stored in the QClog", {
  expect_true(nrow(qclog) == 8)
  expect_true("script/examp-yaml.yaml" %in% qclog$file)
})

# Add test for legacy logs with unresolved file paths
qclog_old <- qclog %>% dplyr::slice(1) %>% dplyr::mutate(file = "script/pk/../data-assembly.R",
                                                         origin = "script/pk/../data-assembly.R")
qclog_combine <- qclog %>% dplyr::slice(1) %>%  dplyr::bind_rows(qclog_old)
readr::write_csv(qclog_combine, "QClog.csv") 

# Confirm legacy QClogs will not show duplicate records for same file with different paths
test_that("logPending only includes resolved paths when unresolved paths present", {
  pendingcsv <- logPending()
  expect_true(nrow(pendingcsv) == 1)
  expect_true(pendingcsv$file == "script/data-assembly.R")
})

