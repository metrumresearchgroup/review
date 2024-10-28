create_test_svn()

logAccept("script/data-assembly/da-functions.R")
logAssign("script/model-summary.Rmd")

da_files <- list.files("script/data-assembly", full.names = TRUE)

list_mulitple <- fileSummary(da_files)
list_singular <- "script/data-assembly/da-functions.R"

test_that("User can provide multiple files", {
  
  expect_equal(length(list_mulitple), 4)
  expect_equal(length(list_singular), 1)
  
  list.i <- list_mulitple[["script/data-assembly/da-study-abc.Rmd"]]
  list.i2 <- list_mulitple[["script/data-assembly/da-functions.R"]]
  
  expect_equal(length(list.i), 4)
  expect_equal(length(list.i2), 4)
  
  expect_true("qclog" %in% names(list.i))
  expect_true("qcstatus" %in% names(list.i))
  expect_true("prevQC" %in% names(list.i))
  expect_true("authors" %in% names(list.i))
  
  expect_true(grepl("No", as.character(list.i$qclog)))
  expect_true(grepl("Yes", as.character(list.i2$qclog)))
  
  expect_equal(length(list.i$authors), 2)
  expect_equal(length(list.i$prevQC), 1)
  
})

test_that("Output shows file has had QC history", {
  
  out1 <- fileSummary("script/data-assembly/da-functions.R")
  out2 <- fileSummary("script/model-summary.Rmd")
  
  expect_equal(length(out1), 1)
  
  expect_true(grepl("QC up to date", 
                    out1$`script/data-assembly/da-functions.R`$qcstatus))
  
  expect_true(grepl("Yes", 
                    out1$`script/data-assembly/da-functions.R`$qclog))
  
  expect_true(length(out1$`script/data-assembly/da-functions.R`$prevQC) == 1)
  
  expect_true(grepl("Needs QC", 
                    out2$`script/model-summary.Rmd`$qcstatus))
  
  expect_true(grepl("Yes", 
                    out2$`script/model-summary.Rmd`$qclog))
  
  expect_true(grepl("No previous QC", 
                    out2$`script/model-summary.Rmd`$prevQC))
  
})
