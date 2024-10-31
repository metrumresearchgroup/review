with_demoRepo({
  
  testthat::test_that("logRemove removes the file from the QClog", {
    
    qclog <- logRead()
    expect_true(nrow(qclog[qclog$file == "script/data-assembly.R",]) > 0)
    
    logRemove("script/data-assembly.R")
    qclog2 <- logRead()
    
    expect_true(nrow(qclog2[qclog2$file == "script/data-assembly.R",]) == 0)
  }) 
})



