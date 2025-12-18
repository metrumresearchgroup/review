# Tests for runWithOutputs

test_that("runWithOutputs reports newly created files", {
  skip_if(Sys.which("fd") == "", "fd command not available")

  root <- here::here()
  tmp_dir <- file.path(root, "tmp_runWithOutputs")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  artifact <- file.path(tmp_dir, "artifact.txt")
  script <- file.path(tmp_dir, "make_artifact.R")

  writeLines(
    c(
      "Sys.sleep(0.1)",
      sprintf("writeLines('test output', '%s')", artifact)
    ),
    script
  )

  out <- testthat::capture_output(runWithOutputs(script))

  expect_true(file.exists(artifact))
  expect_match(out, "outputs:")
  expect_match(out, "tmp_runWithOutputs/artifact.txt")
})
