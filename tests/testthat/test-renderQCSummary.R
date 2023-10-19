repo <- demoRepo("abc-123")
setwd(repo)

if (Sys.getenv("METWORX_VERSION") != "") {
  test_that("renderQCSummary works with valid directory", {
    
    expect_silent({
      renderQCSummary(.dir = logRoot(), .output_dir = logRoot())
    })
    
    # Check that the output file was created
    expect_true(file.exists(file.path(logRoot(), paste0("qc-summary-", Sys.Date(), ".pdf"))))
    
  })
    
  test_that("renderQCSummary doesn't save file if directory not given", {
    
    temp_dir <- tempdir()
    
    expect_silent({
      renderQCSummary(.dir = logRoot())
    })
    
    # Check that the output file was created
    expect_true(file.exists(file.path(temp_dir, paste0("qc-summary-", Sys.Date(), ".pdf"))))
    
  })
}

