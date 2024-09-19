if (Sys.getenv("METWORX_VERSION") != "") {
  
  create_test_svn()
  
  logAssign("script/model-management.R")
  logAccept("script/box-sample-code.R")
  
  test_that("renderQCReport works with valid directory", {
    
    renderQCReport(.output_dir = logRoot())
    
    # Check that the output file was created
    expect_true(file.exists(file.path(logRoot(), paste0("qc-report-", Sys.Date(), ".pdf"))))
    
  })
}
