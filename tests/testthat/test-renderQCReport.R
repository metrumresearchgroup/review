if (Sys.getenv("METWORX_VERSION") != "") {
  
  create_test_svn()
  
  logAssign("script/model-management.R")
  logAccept("script/box-sample-code.R")
  
  test_that("renderQCReport errors if .output_dir is missing", {
    expect_error(renderQCReport(), "'.output_dir' is required")
  })
  
  test_that("renderQCReport works with valid directory and default project_number", {
    renderQCReport(.output_dir = logRoot())
    # project_number parameter inside the report comes from SVN URL (e.g., "review-tests")
    # report_suffix (used in file name) is derived from the last part of the SVN URL (e.g., "tests")
    expected_file <- file.path(logRoot(), paste0("tests-qc-report-", Sys.Date(), ".pdf"))
    expect_true(file.exists(expected_file))
  })
  
  test_that("renderQCReport works with explicit project_number", {
    renderQCReport(.output_dir = logRoot(), .project_number = "abc-study-ONE")
    # PDF file uses provided .project_number (lowercased)
    # project parameter in report still comes from SVN
    expected_file <- file.path(logRoot(), paste0(tolower("abc-study-ONE"), "-qc-report-", Sys.Date(), ".pdf"))
    expect_true(file.exists(expected_file))
  })
  
  test_that("renderQCReport errors if directory does not exist", {
    expect_error(
      renderQCReport(.output_dir = tempfile()),
      "does not exist"
    )
  })
  
  test_that("renderQCReport works with project_number with no dash", {
    renderQCReport(.output_dir = logRoot(), .project_number = "SINGLE")
    # PDF file uses provided .project_number (lowercased)
    # project parameter in report still comes from SVN
    expected_file <- file.path(logRoot(), paste0("single-qc-report-", Sys.Date(), ".pdf"))
    expect_true(file.exists(expected_file))
  })
  
  test_that("renderQCReport errors gracefully if QC log does not exist", {
    file.remove("QClog.csv")
    expect_error(
      renderQCReport(.output_dir = tempfile()),
      "QC log does not exist"
    )
  })
  
  test_that("renderQCReport errors gracefully if QC log does not exist", {
    logCreate()
    expect_error(
      renderQCReport(.output_dir = tempfile()),
      "QC log is empty"
    )
  })
}
