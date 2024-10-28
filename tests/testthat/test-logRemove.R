with_demoRepo({
  
  logRemove("script/data-assembly.R")
  
  testthat::test_that("logRemove removes the file from the QClog", {
    qclog <- logRead()
    
    expect_true(nrow(qclog[qclog$file == "script/data-assembly.R",]) == 0)
  }) 
})



