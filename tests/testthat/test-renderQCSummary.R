with_demoRepo({
  if (Sys.getenv("METWORX_VERSION") != "") {
    test_that("renderQCSummary works with valid directory", {
      
      expect_message({
        renderQCSummary(.output_dir = logRoot())
      })
      
      # Check that the output file was created
      expect_true(file.exists(file.path(logRoot(), paste0("qc-summary-", Sys.Date(), ".pdf"))))
      
    })
    
    test_that("renderQCSummary doesn't save file if directory not given", {
      
      temp_dir <- tempdir()
      
      expect_message({
        renderQCSummary()
      })
      
      # Check that the output file was created
      expect_true(file.exists(file.path(temp_dir, paste0("qc-summary-", Sys.Date(), ".pdf"))))
      
    })
  }
})



