with_demoRepo({
  file1 <- "script/data-assembly.R"
  file_fullyQCed <- "script/combine-da.R"
  
  test_that("diffQced produces error message if no previous QC", {
    expect_error(diffQced("script/examp-yaml.yaml"))
    expect_error(diffQced("script/examp-txt.txt"))
  })
  
  diffqc <- diffQced(file_fullyQCed) %>% suppressMessages()
  
  test_that("diffQced reports no difference between identical files", {
    expect_true(is.null(diffqc))
  })
  
  writeLines(c("Modified text"),
             "script/examp-yaml.yaml")
  test_that("diffQced throws a warning if the file hasn't been updated with svn up", {
    expect_error(diffqc <- diffQced("examp-txt.txt"))
  })
  
  diffqc <- diffQced(file1) %>% suppressMessages()
  
  test_that("diffQced identifies difference between local and QCed file version", {
    expect_true(length(diffqc@target) == 5)
    expect_true(length(diffqc@current) == 10)
    expect_equal(diffqc@target[1], diffqc@current[1])
  })
  
  this_test_user <- Sys.info()[["user"]]
  expect_message(diffQced(file1), glue::glue("'{this_test_user}' has modified '{basename(file1)}' since last QC"))
  
})

