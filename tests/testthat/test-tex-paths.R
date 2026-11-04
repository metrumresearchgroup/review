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
    getTablePathsFromTex(tables_tex(),
      .macros = c(finalPKRunno = "106", basePKRunno = "100")),
    c(
      expected_path("deliv/table/report/pk-data-sum.tex"),
      expected_path("deliv/table/report/base-param-fixed.tex"),
      expected_path("deliv/table/report/base-param-106-tab.tex"),
      expected_path("deliv/table/report/100-param-fixed.tex")
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
    getTablePathsFromTex(tables_tex(),
      .macros = c(finalPKRunno = "106", basePKRunno = "100"))
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

tex_test_report <- function() {
  root <- normalizePath(withr::local_tempdir(.local_envir = parent.frame()), winslash = "/")
  list(root = root, write = function(path, text = character()) {
    file <- file.path(root, path)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, file)
    file
  })
}

test_that("balanced multiline and starred commands expand macros in source order", {
  report <- tex_test_report()
  report$write("preamble.tex", c(
    "\\newcommand{\\runNumber}{106}",
    "\\newcommand{\\plotPath}[1]{images/#1-\\runNumber}",
    "\\newcommand{\\tablePath}[2][summary]{tables/#1-#2}",
    "\\def\\suffix{final}",
    "\\newcommand{\\unused}{\\includegraphics{not-used.png}}"))
  report$write("chapters/results.tex", c(
    "\\includegraphics*", "[width=.8\\textwidth]", "{\\plotPath{fit}.png}",
    "\\begin{table}", "\\input", "{\\tablePath{\\suffix}}", "\\end{table}",
    "\\renewcommand{\\runNumber}{107}",
    "\\includegraphics{\\plotPath{fit}.png}"))
  main <- report$write("main.tex", c("\\input{preamble}", "\\input{chapters/results}"))
  expect_equal(getFigurePathsFromTex(main), file.path(report$root,
    c("images/fit-106.png", "images/fit-107.png")))
  expect_equal(getTablePathsFromTex(main), file.path(report$root, "tables/summary-final.tex"))
  expect_warning(paths <- getFigurePathsFromTex(report$write("unknown.tex",
    "\\includegraphics{plot-\\undefined.png}")), "Unresolved TeX macro")
  expect_identical(paths, character())
  expect_equal(getFigurePathsFromTex(file.path(report$root, "unknown.tex"),
    .macros = c(undefined = "42")), file.path(report$root, "plot-42.png"))
})

test_that("graphics search paths and declared extensions find existing images", {
  report <- tex_test_report()
  report$write("images/plot.png")
  report$write("images/plot.pdf")
  report$write("other/second.png")
  main <- report$write("main.tex", c(
    "\\newcommand{\\imageDir}{images/}",
    "\\graphicspath{{\\imageDir}{other/}}",
    "\\includegraphics{plot}",
    "\\DeclareGraphicsExtensions{.png,.pdf}",
    "\\includegraphics*{plot}",
    "\\includegraphics{second.png}"))
  expect_equal(getFigurePathsFromTex(main), file.path(report$root,
    c("images/plot.pdf", "images/plot.png", "other/second.png")))
  missing <- report$write("missing.tex", "\\includegraphics{not-built}")
  expect_warning(paths <- getFigurePathsFromTex(missing), "Cannot determine image extension")
  expect_equal(paths, file.path(report$root, "not-built"))
})

test_that("imports and subimports resolve files and their assets in imported directories", {
  report <- tex_test_report()
  main <- report$write("main.tex", "\\import{chapters/}{results}")
  report$write("chapters/results.tex", c(
    "\\includegraphics{plot.png}",
    "\\begin{table}\\input{summary}\\end{table}",
    "\\subimport{details/}{more}"))
  report$write("chapters/details/more.tex", c(
    "\\includegraphics{detail.pdf}",
    "\\begin{table}\\input{detail-table}\\end{table}"))
  expect_equal(getFigurePathsFromTex(main), file.path(report$root,
    c("chapters/plot.png", "chapters/details/detail.pdf")))
  expect_equal(getTablePathsFromTex(main), file.path(report$root,
    c("chapters/summary.tex", "chapters/details/detail-table.tex")))
})

test_that("inactive branches, includeonly, and verbatim examples do not add assets", {
  report <- tex_test_report()
  main <- report$write("main.tex", c(
    "\\newif\\ifdraft", "\\draftfalse",
    "\\ifdraft", "\\includegraphics{draft.png}", "\\else",
    "\\includegraphics{final.png}", "\\fi",
    "\\iffalse", "\\iftrue", "\\includegraphics{nested-hidden.png}",
    "\\fi", "\\else", "\\input{tables/visible}", "\\fi",
    "\\includeonly{chapters/yes}",
    "\\include{chapters/no}", "\\include{chapters/yes}",
    "\\input{chapters/always}",
    "\\begin{verbatim}", "\\includegraphics{example.png}", "\\end{verbatim}",
    "\\verb|\\input{tables/example}|",
    "% \\includegraphics{comment.png}",
    "\\includegraphics{percent\\%.png} % trailing comment",
    "\\includegraphics% comment between command and argument", "{joined.png}"))
  report$write("chapters/no.tex", "\\includegraphics{excluded.png}")
  report$write("chapters/yes.tex", "\\includegraphics{included.png}")
  report$write("chapters/always.tex", "\\includegraphics{always.png}")
  expect_equal(getFigurePathsFromTex(main), file.path(report$root,
    c("final.png", "included.png", "always.png", "percent%.png", "joined.png")))
  expect_equal(getTablePathsFromTex(main), file.path(report$root, "tables/visible.tex"))
})

test_that("arbitrary section directories distinguish wrappers from table leaves", {
  report <- tex_test_report()
  main <- report$write("main.tex", c("\\input{chapters/intro}", "\\input{odd/empty}",
    "\\input{odd/not-created}"))
  report$write("chapters/intro.tex", "\\input{assets/table}")
  report$write("assets/table.tex", "\\begin{tabular}{c}A\\end{tabular}")
  report$write("odd/empty.tex", "No figures or tables yet.")
  expect_equal(getTablePathsFromTex(main, .section_dirs = c("sections", "odd")),
    file.path(report$root, "assets/table.tex"))
})

test_that("conditional tests, empty includeonly, and recursive macros are explicit", {
  report <- tex_test_report()
  report$write("excluded.tex", "\\includegraphics{excluded.png}")
  main <- report$write("main.tex", c(
    "\\def\\run{106}",
    "\\ifdefined\\run", "\\includegraphics{defined.png}", "\\fi",
    "\\ifthenelse{\\equal{\\run}{106}}{\\includegraphics{equal.png}}{\\includegraphics{wrong.png}}",
    "\\newif\\ifdraft", "\\drafttrue",
    "\\ifthenelse{\\boolean{draft}}{\\input{tables/draft}}{\\input{tables/final}}",
    "\\includeonly{}", "\\include{excluded}"))
  expect_equal(getFigurePathsFromTex(main), file.path(report$root, c("defined.png", "equal.png")))
  expect_equal(getTablePathsFromTex(main), file.path(report$root, "tables/draft.tex"))
  recursive <- report$write("recursive.tex", c("\\def\\loop{\\loop}", "\\input{\\loop}"))
  expect_warning(paths <- getTablePathsFromTex(recursive), "Recursive TeX macro")
  expect_identical(paths, character())
  unsupported <- report$write("unsupported.tex", c(
    "\\ifnum1>0", "\\includegraphics{unknown.png}", "\\else",
    "\\includegraphics{also-unknown.png}", "\\fi", "\\includegraphics{known.png}"))
  expect_warning(paths <- getFigurePathsFromTex(unsupported), "Unsupported TeX conditional")
  expect_equal(paths, file.path(report$root, "known.png"))
})

test_that("structural sections with inline tabular content remain wrappers", {
  report <- tex_test_report()
  main <- report$write("main.tex", "\\input{chapters/results}")
  report$write("chapters/results.tex", c(
    "\\section{Results}", "\\begin{tabular}{c}Inline\\end{tabular}",
    "\\input{tables/summary}"))
  expect_equal(getTablePathsFromTex(main), file.path(report$root, "tables/summary.tex"))
  imported <- report$write("imported.tex", "\\begin{table}\\import{assets/}{summary}\\end{table}")
  report$write("assets/summary.tex", "\\begin{tabular}{c}A\\end{tabular}")
  expect_equal(getTablePathsFromTex(imported), file.path(report$root, "assets/summary.tex"))
})

test_that("commented verbatim delimiters cannot hide live references", {
  report <- tex_test_report()
  main <- report$write("main.tex", c(
    "% \\begin{verbatim}", "\\includegraphics{visible.png}",
    "% \\end{verbatim}",
    "\\begin{verbatim}", "% \\includegraphics{hidden.png}", "\\end{verbatim}",
    "\\\\% row ending before a comment", "\\input{tables/visible}"))
  expect_equal(getFigurePathsFromTex(main), file.path(report$root, "visible.png"))
  expect_equal(getTablePathsFromTex(main), file.path(report$root, "tables/visible.tex"))
})
