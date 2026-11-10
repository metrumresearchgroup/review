latex_test_report <- function() {
  skip_if(!nzchar(Sys.which("pdflatex")), "pdfLaTeX is not installed")
  root <- normalizePath(withr::local_tempdir(.local_envir = parent.frame()), winslash = "/")
  list(root = root, write = function(path, text = character()) {
    file <- file.path(root, path)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, file)
    file
  }, image = function(path) {
    file <- file.path(root, path)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    grDevices::pdf(file, width = 2, height = 2)
    graphics::plot.new()
    graphics::text(.5, .5, "Figure")
    grDevices::dev.off()
    file
  })
}

test_that("one main path finds actual assets through LaTeX rather than folder rules", {
  report <- latex_test_report()
  report$image("assets/images/plot.pdf")
  report$image("assets/images/second.pdf")
  report$image("assets/images/probed-only.pdf")
  report$write("localstyle.sty", c("\\ProvidesPackage{localstyle}",
    "\\newcommand{\\useplot}[1]{\\includegraphics[width=1cm]{#1}}"))
  main <- report$write("main.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}",
    "\\usepackage{graphicx}", "\\usepackage{localstyle}",
    "\\graphicspath{{assets/images/}}", "\\includeonly{chapters/results}",
    "\\begin{document}", "\\IfFileExists{assets/images/probed-only.pdf}{}{}",
    "\\include{chapters/excluded}",
    "\\include{chapters/results}", "\\end{document}"))
  report$write("chapters/results.tex", c(
    "\\section{Results}", "\\ifnum1>0", "\\useplot{plot}",
    "\\else", "\\includegraphics{does-not-exist}", "\\fi",
    "\\begingroup\\def\\image{second}\\useplot{\\image}\\endgroup",
    "\\useplot{plot}", "\\begin{table}\\input{data/summary}\\end{table}",
    "\\input{data/standalone}", "\\input{chapters/notes}",
    "\\verb|\\includegraphics{example-only}|",
    "% \\input{data/commented}"))
  report$write("chapters/notes.tex", "Just prose, not a table.")
  report$write("data/summary.tex", "\\begin{tabular}{c}\\input{data/rows}\\end{tabular}")
  report$write("data/rows.tex", "Summary")
  report$write("data/standalone.tex", "\\begin{tabular}{c}Standalone\\end{tabular}")
  before <- list.files(report$root, recursive = TRUE, all.files = TRUE)

  assets <- getAssetPathsFromTex(main)
  expect_named(assets, c("figures", "tables"))
  expect_equal(assets$figures,
    file.path(report$root, c("assets/images/plot.pdf", "assets/images/second.pdf")))
  expect_equal(assets$tables,
    file.path(report$root, c("data/summary.tex", "data/standalone.tex")))
  expect_equal(list.files(report$root, recursive = TRUE, all.files = TRUE), before)
})

test_that("compilation resolves source paths containing spaces and percent signs", {
  report <- latex_test_report()
  main <- report$write("report % draft.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}",
    "\\begin{document}", "\\begin{table}\\input{data/table}\\end{table}",
    "\\end{document}"))
  report$write("data/table.tex", "\\begin{tabular}{c}Table\\end{tabular}")
  expect_equal(getAssetPathsFromTex(main)$tables, file.path(report$root, "data/table.tex"))
})

test_that("failed builds error instead of returning a partial asset list", {
  report <- latex_test_report()
  main <- report$write("main.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}",
    "\\begin{document}", "\\begin{table}\\input{missing}\\end{table}",
    "\\end{document}"))
  expect_error(getAssetPathsFromTex(main)$tables, "Could not compile.*no partial", fixed = FALSE)
})

test_that("inline tables do not invent external asset paths", {
  report <- latex_test_report()
  main <- report$write("main.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}",
    "\\begin{document}", "\\begin{tabular}{c}Inline\\end{tabular}",
    "\\end{document}"))
  expect_identical(getAssetPathsFromTex(main)$tables, character())
})

test_that("inline section tables do not hide later external table assets", {
  report <- latex_test_report()
  main <- report$write("main.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}",
    "\\begin{document}", "\\input{sections/results}", "\\end{document}"))
  report$write("sections/results.tex", c(
    "\\begin{tabular}{c}\\input{data/inline-rows}\\end{tabular}",
    "\\begin{table}\\input{data/summary}\\end{table}",
    "\\input{data/standalone}"))
  report$write("data/inline-rows.tex", "Inline")
  report$write("data/summary.tex", "\\begin{tabular}{c}\\input{data/summary-rows}\\end{tabular}")
  report$write("data/summary-rows.tex", "Summary")
  report$write("data/standalone.tex", "\\begin{tabular}{c}Standalone\\end{tabular}")

  expect_equal(getAssetPathsFromTex(main)$tables,
    file.path(report$root, c("sections/results.tex", "data/summary.tex", "data/standalone.tex")))
})

test_that("top-level table files report later inputs that contain their own tabular", {
  # Without a table environment, a table file and a section with an inline table
  # look the same in the trace; prefer over-reporting to silently dropping assets.
  report <- latex_test_report()
  main <- report$write("main.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}",
    "\\begin{document}", "\\input{data/summary}", "\\end{document}"))
  report$write("data/summary.tex", c(
    "\\begin{tabular}{c}\\input{data/summary-rows}\\end{tabular}",
    "\\input{data/continued}"))
  report$write("data/summary-rows.tex", "Summary")
  report$write("data/continued.tex", "\\begin{tabular}{c}Continued\\end{tabular}")

  expect_equal(getAssetPathsFromTex(main)$tables,
    file.path(report$root, c("data/summary.tex", "data/continued.tex")))
})

test_that("runtime input search paths disambiguate identically named table files", {
  report <- latex_test_report()
  report$write("first/shared.tex", "\\begin{tabular}{c}First\\end{tabular}")
  report$write("second/shared.tex", "\\begin{tabular}{c}Second\\end{tabular}")
  main <- report$write("main.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}", "\\begin{document}",
    "\\makeatletter\\def\\input@path{{first/}}\\makeatother", "\\input{shared}",
    "\\makeatletter\\def\\input@path{{second/}}\\makeatother", "\\input{shared}",
    "\\end{document}"))
  expect_equal(getAssetPathsFromTex(main)$tables,
    file.path(report$root, c("first/shared.tex", "second/shared.tex")))
})

test_that("table assets are identified by runtime use, not filename extensions", {
  report <- latex_test_report()
  report$write("assets/data.dat", "\\begin{tabular}{c}Data\\end{tabular}")
  main <- report$write("main.tex", c(
    "% !TeX program = pdflatex", "\\documentclass{article}",
    "\\begin{document}", "\\input{assets/data.dat}", "\\end{document}"))
  expect_equal(getAssetPathsFromTex(main)$tables, file.path(report$root, "assets/data.dat"))
})
