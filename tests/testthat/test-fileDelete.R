with_demoRepo({
  
  fileDelete("script/data-assembly.R")
  
  testthat::test_that("fileDelete deletes locally and in SVN history", {
    expect_true(!file.exists("script/data-assembly.R"))
    
    status <- 
      dplyr::bind_rows(x$target) %>% 
      dplyr::filter(.attrs == "script/data-assembly.R") %>% 
      unlist()
    
    expect_true(status[["wc-status..attrs.item"]] == "deleted")
  }) 
  
  testthat::test_that("fileDelete removes the file from the QClog", {
    qclog <- logRead()
    
    expect_true(nrow(qclog[qclog$file == "script/data-assembly.R",]) == 0)
  }) 
})



