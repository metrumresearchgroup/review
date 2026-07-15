fixture_root <- function() {
  testthat::test_path("fixtures", "report-tex")
}

expected_path <- function(path) {
  abs <- normalizePath(
    file.path(fixture_root(), path),
    winslash = "/",
    mustWork = FALSE
  )
  roots <- unique(normalizePath(
    c(getwd(), here::here()),
    winslash = "/",
    mustWork = FALSE
  ))
  rel_roots <- roots[startsWith(abs, paste0(roots, "/")) | abs == roots]
  if (!length(rel_roots)) {
    return(abs)
  }
  root <- rel_roots[[which.max(nchar(rel_roots))]]
  sub(
    paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", root), "/?"),
    "",
    abs
  )
}

figures_tex <- function() {
  file.path(fixture_root(), "deliv", "report", "sections", "figures.tex")
}

tables_tex <- function() {
  file.path(fixture_root(), "deliv", "report", "sections", "tables.tex")
}

test_that("getFigurePathsFromTex returns stable relative paths", {
  testthat::expect_equal(
    getFigurePathsFromTex(figures_tex()),
    c(
      expected_path("deliv/figure/eda/cont-vs-cont.png"),
      expected_path("deliv/figure/pk-vpc-106-dose-norm.png"),
      expected_path("deliv/figure/106/106-cwres-combined.pdf"),
      expected_path("deliv/figure/pk/pk-conc-time.png")
    )
  )
})

test_that("getTablePathsFromTex returns stable relative paths and adds .tex extension", {
  testthat::expect_equal(
    getTablePathsFromTex(tables_tex()),
    c(
      expected_path("deliv/table/report/pk-data-sum.tex"),
      expected_path("deliv/table/report/base-param-fixed.tex")
    )
  )
})

test_that("commented-out entries are ignored", {
  testthat::expect_false(any(grepl(
    "commented-out",
    getFigurePathsFromTex(figures_tex())
  )))
  testthat::expect_false(any(grepl(
    "commented-out",
    getTablePathsFromTex(tables_tex())
  )))
})
