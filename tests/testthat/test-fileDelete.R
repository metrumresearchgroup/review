with_demoRepo({
  
  fileDelete("script/data-assembly.R")
  
  testthat::test_that("fileDelete deletes locally and in SVN history", {
    expect_true(!file.exists("script/data-assembly.R"))
    expect_error(svnLog("script/data-assembly.R"))
  }) 
  
  testthat::test_that("fileDelete removes the file from the QClog", {
    qclog <- logRead()
    
    expect_true(nrow(qclog[qclog$file == "script/data-assembly.R",]) == 0)
  }) 
})



