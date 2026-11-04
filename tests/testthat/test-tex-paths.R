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
      expected_path("deliv/table/report/base-param-fixed.tex"),
      expected_path("deliv/table/report/base-param-finalPKRunno-tab.tex"),
      expected_path("deliv/table/report/basePKRunno-param-fixed.tex")
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


test_that("report-root includes collect nested assets without returning wrappers", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  report <- file.path(root, "report")
  dir.create(report)
  dir.create(file.path(report, "sections"))
  dir.create(file.path(root, "table"))
  write_tex <- function(path, lines) writeLines(lines, file.path(report, path))
  write_tex("main.tex", c(
    "\\input{sections/figures}", "\\input{sections/tables}",
    "\\include{sections/methods}", "\\input{sections/empty}"))
  write_tex("sections/figures.tex", "\\input{sections/figures-child.tex}")
  write_tex("sections/figures-child.tex", c(
    "\\includegraphics{../figure/plot.png}",
    "\\input{sections/deeper}", "\\input{sections/missing}"))
  write_tex("sections/deeper.tex", c(
    "\\includegraphics{../figure/nested.pdf}",
    "\\input{sections/figures}"))
  write_tex("sections/tables.tex", c(
    "\\input{sections/tables-child}", "\\input{sections/missing}"))
  write_tex("sections/tables-child.tex", c(
    "\\input{../table/existing}", "\\input{../table/missing}",
    "\\input{sections/tables}", "% \\input{../table/commented}"))
  writeLines("\\input{implementation-detail}", file.path(root, "table/existing.tex"))
  write_tex("sections/methods.tex", c(
    "\\input{../table/methods.tex}",
    "\\includegraphics{../figure/plot.png}"))
  write_tex("sections/empty.tex", character())
  asset <- function(path) normalizePath(file.path(root, path),
    winslash = "/", mustWork = FALSE)

  expect_equal(getFigurePathsFromTex(file.path(report, "sections/figures.tex")),
    c(asset("figure/plot.png"), asset("figure/nested.pdf")))
  expect_equal(getTablePathsFromTex(file.path(report, "sections/tables.tex")),
    c(asset("table/existing.tex"), asset("table/missing.tex")))
  expect_equal(getFigurePathsFromTex(file.path(report, "main.tex")),
    c(asset("figure/plot.png"), asset("figure/nested.pdf")))
  expect_equal(getTablePathsFromTex(file.path(report, "main.tex")),
    c(asset("table/existing.tex"), asset("table/missing.tex"),
      asset("table/methods.tex")))
})

test_that("explicit report root takes precedence over sibling includes", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  dir.create(file.path(root, "sections"))
  dir.create(file.path(root, "elsewhere"))
  entry <- file.path(root, "elsewhere/entry.tex")
  writeLines("\\input{sections/child}", entry)
  writeLines(c("\\includegraphics{correct.png}", "\\input{table.tex}"),
    file.path(root, "sections/child.tex"))
  dir.create(file.path(root, "elsewhere/sections"))
  writeLines("\\includegraphics{wrong.png}",
    file.path(root, "elsewhere/sections/child.tex"))
  expect_equal(getFigurePathsFromTex(entry, .report_dir = root),
    file.path(normalizePath(root), "correct.png"))
  expect_equal(getTablePathsFromTex(entry, .report_dir = root),
    file.path(normalizePath(root), "table.tex"))

  writeLines("\\input{child}", file.path(root, "sections/tables.tex"))
  expect_equal(getTablePathsFromTex(file.path(root, "sections/tables.tex")),
    file.path(normalizePath(root), "table.tex"))
})
