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
  
  test_that("renderQCSummary finds expected files", {
    
    # --- Mock Setup ---
    mock_svnProjInfo <- function(...) {
      list(
        url = "svn+ssh://test@example.com/repo/test",
        rev = 123,
        this_svn_user = "test",
        host = "example.com",
        rev_author = "test",
        rev_datetime = Sys.time()
      )
    }
    
    mock_repoHistory <- function() {
      dplyr::tibble(
        file = "script/test-missing-from-log.R",
        author = "test",
        date = Sys.Date(),
        rev = 123,
        msg = "test commit"
      )
    }
    
    # --- Apply Mocks ---
    # local_mocked_bindings safely replaces functions in the package namespace
    # and automatically restores them when this test block finishes.
    testthat::local_mocked_bindings(
      svnProjInfo = mock_svnProjInfo,
      repoHistory = mock_repoHistory,
      .package = "review"
    )
    
    with_demoRepo({
      # 1. Setup: Create the file on disk
      target_file <- "script/test-missing-from-log.R"
      writeLines("# meaningful content", target_file)
      
      # 2. Render
      output_dir <- tempdir()
      renderQCSummary(.output_dir = output_dir)
      
      # 3. Validation
      pdf_name <- paste0("qc-summary-", Sys.Date(), ".pdf")
      pdf_path <- file.path(output_dir, pdf_name)
      
      expect_true(file.exists(pdf_path))
      
      pdf_content <- pdftools::pdf_text(pdf_path)
      combined_text <- paste(pdf_content, collapse = " ")
      
      expect_true(grepl("test-missing-from-log.R", combined_text, fixed = TRUE))
    })
  })
}
