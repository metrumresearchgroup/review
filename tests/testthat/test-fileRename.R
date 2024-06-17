with_demoRepo({
  
  fileRename("script/data-assembly.R", .new_filepath = "script/data-assembly2.R")
  fileRename("script/combine-da.R", .new_filepath = "script/pk/combine-da.R")
  
  testthat::test_that("fileRename renames file locally and in SVN history", {
    expect_true(file.exists("script/data-assembly2.R"))
    expect_true(!file.exists("script/data-assembly.R"))
    
    expect_true(nrow(svnLog("script/data-assembly2.R")) > 1)
  }) 
  
  testthat::test_that("fileRename updates the file name in the QClog", {
    qclog <- logRead()
    
    expect_true(nrow(qclog[qclog$file == "script/data-assembly2.R",]) > 0)
    expect_true(nrow(qclog[qclog$file == "script/data-assembly.R",]) == 0)
  }) 
  
  testthat::test_that("fileRename can move files to different directories", {
    expect_true(nrow(svnLog("script/pk/combine-da.R")) > 1)
    
    expect_true(!all(list.files("script") == "combine-da.R"))
    
    expect_true(file.exists("script/pk/combine-da.R"))
    expect_true(!file.exists("script/combine-da.R"))
  }) 
  
})


