with_demoRepo({
  
  logDelete("script/data-assembly.R")
  
  testthat::test_that("fileDelete removes the file from the QClog", {
    qclog <- logRead()
    
    expect_true(nrow(qclog[qclog$file == "script/data-assembly.R",]) == 0)
  }) 
})



