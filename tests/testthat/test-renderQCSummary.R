if (Sys.getenv("METWORX_VERSION") != "") {
  
  create_test_svn()
  
  logAccept("script/data-assembly/da-combine-studies.Rmd")
  logAssign("script/data-assembly/da-functions.R")
  
  test_that("renderQCSummary works with valid directory", {
    
    expect_null({
      renderQCSummary(.output_dir = logRoot())
    })
    
    # Check that the output file was created
    expect_true(file.exists(file.path(logRoot(), paste0("qc-summary-", Sys.Date(), ".pdf"))))
    
  })
  
  test_that("renderQCSummary doesn't save file if directory not given", {
    
    temp_dir <- tempdir()
    
    expect_null({
      renderQCSummary()
    })
    
    # Check that the output file was created
    expect_true(file.exists(file.path(temp_dir, paste0("qc-summary-", Sys.Date(), ".pdf"))))
    
  })
  
  test_that("renderQCSummary errors gracefully if QC log does not exist", {
    file.remove("QClog.csv")
    expect_error(
      renderQCSummary(),
      "QC log does not exist"
    )
  })
}




