if (Sys.getenv("METWORX_VERSION") != "") {
  
  create_test_svn()
  
  test_that("renderOutputs errors if .output_dir is missing", {
    expect_error(renderOutputs(), "'.output_dir' is required")
  })
  
  test_that("renderOutputs errors if directory does not exist", {
    expect_error(
      renderOutputs(.output_dir = tempfile()),
      "does not exist"
    )
  })
  
  test_that("renderOutputs errors gracefully if QC log does not exist", {
    file.remove("QClog.csv")
    expect_error(
      renderOutputs(.output_dir = tempfile()),
      "QC log does not exist"
    )
  })
}
