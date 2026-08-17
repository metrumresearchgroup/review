with_demoRepo({
  test_that("svnStatus returns a data frame with expected columns", {
    s <- svnStatus("deliv/figure")
    expect_s3_class(s, "data.frame")
    expect_named(s, c("path", "status"))
    expect_type(s$path, "character")
    expect_type(s$status, "character")
  })

  test_that("svnStatus detects modified files", {
    s <- svnStatus("deliv/figure")
    expect_true("example-pdf1.pdf" %in% s$path)
    modified <- s$path[s$status == "modified"]
    expect_true("example-pdf1.pdf" %in% modified)
  })

  test_that("svnStatus detects unversioned files", {
    s <- svnStatus("deliv/figure")
    unversioned <- s$path[s$status == "unversioned"]
    expect_true("example-pdf4.pdf" %in% unversioned)
  })

  test_that("svnStatus returns zero-row data frame for a clean directory", {
    s <- svnStatus("script/pk")
    expect_s3_class(s, "data.frame")
    expect_equal(nrow(s), 0L)
  })
})
