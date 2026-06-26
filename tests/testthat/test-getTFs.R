file <- system.file("review-tabs-figs.pdf", package = "review")

tfs <- getTFs(.file = file)

testthat::test_that("getTFs returns a named list of data frames", {
  expect_named(tfs, c("pdf_and_tex", "pdf_only", "tex_only"))
  expect_s3_class(tfs$pdf_and_tex, "data.frame")
  expect_s3_class(tfs$pdf_only, "data.frame")
  expect_s3_class(tfs$tex_only, "data.frame")

  # No TeX file for this fixture, so all rows land in pdf_only
  expect_equal(nrow(tfs$pdf_and_tex), 0L)
  expect_equal(nrow(tfs$tex_only), 0L)

  expect_identical(
    names(tfs$pdf_only),
    c("page", "type", "source_graphic", "source_code", "source_graphic_exists", "source_code_exists")
  )
  expect_identical(
    names(tfs$tex_only),
    c("type", "source_graphic", "source_graphic_exists")
  )

  expect_true(nrow(dplyr::distinct(tfs$pdf_only, source_graphic, source_code)) == nrow(tfs$pdf_only))
})

test_that("getTFs parses tables/figures and pairs them with the correct source code", {

  pdf <- system.file("review-tabs-figs.pdf", package = "review")
  expect_true(file.exists(pdf))

  res <- getTFs(pdf)
  # No TeX file — all rows in pdf_only
  df <- res$pdf_only

  expect_s3_class(df, "data.frame")
  expect_false("idx" %in% names(df))

  # should only include rows with a source_graphic (tables + figures), not "Source code" rows
  expect_false(any(is.na(df$source_graphic)))
  expect_false(any(is.na(df$source_code)))

  # this example PDF has 7 Source file entries + 4 Source graphic entries = 11
  expect_equal(nrow(df), 11)

  # ordering + correct pairing (first chunk uses eda-tables-1.R)
  expect_equal(df$source_code[[1]], "script/pk/eda-tables-1.R")
  expect_equal(df$page[[1]], 2)
  expect_equal(df$type[[1]], "table")
  expect_equal(df$source_graphic[[1]], "deliv/table/eda/id-sum-example-1.tex")

  # last row is the final figure, paired with pk-eda-figures-2.R
  expect_equal(df$source_code[[nrow(df)]], "script/pk/pk-eda-figures-2.R")
  expect_equal(df$source_graphic[[nrow(df)]], "deliv/figure/wt-mpg-scatter-2.pdf")
  expect_equal(df$page[[nrow(df)]], 11)
  expect_equal(df$type[[nrow(df)]], "figure")

  # spot-check a figure row that is known to have unicode minus in the PDF text
  expect_true(any(df$source_code == "script/pk/pk-eda-figures.R"))
  expect_true(any(df$source_graphic == "deliv/figure/cont-vs-cont.pdf"))
})

test_that("getTFs parses source info embedded in PNG figures in reports", {
  report <- file.path("fixtures", "figures", "report.pdf")
  if (!file.exists(report)) {
    report <- file.path("tests", "testthat", "fixtures", "figures", "report.pdf")
  }
  testthat::expect_true(file.exists(report))
  testthat::skip_if(!nzchar(Sys.which("pdfimages")), "pdfimages CLI not available")

  expected_tfs <- tibble::tribble(
    ~page, ~type, ~source_graphic, ~source_code,
    15L, "table", "study-summary-table.tex", "study-summary.R",
    33L, "table", "deliv/table/report/pk-data-sum.tex", "script/eda-tables.R",
    34L, "table", "deliv/table/report/pk-data-sum-per-dose.tex", "script/eda-tables.R",
    35L, "table", "deliv/table/report/rf-per-dose.tex", "script/eda-tables.R",
    36L, "table", "deliv/table/report/hepatic-per-dose.tex", "script/eda-tables.R",
    37L, "table", "deliv/table/report/cont-covar-sum.tex", "script/eda-tables.R",
    38L, "table", "deliv/table/report/cont-covar-sum-rf.tex", "script/eda-tables.R",
    39L, "table", "deliv/table/report/cont-covar-sum-hepatic.tex", "script/eda-tables.R",
    40L, "table", "deliv/table/report/base-param-fixed.tex", "script/pk-base-model-table.R",
    40L, "table", "deliv/table/report/base-param-random.tex", "script/pk-base-model-table.R",
    41L, "table", "deliv/table/report/final-param-fixed-boot.tex", "script/pk-final-model-table-boot.R",
    42L, "table", "deliv/table/report/final-param-random-boot.tex", "script/pk-final-model-table-boot.R",
    43L, "figure", "deliv/figure/eda/cont-vs-cont.png", "script/eda-figures.R",
    44L, "figure", "deliv/figure/eda/cat-vs-cont-rf.png", "script/eda-figures.R",
    45L, "figure", "deliv/figure/eda/cat-vs-cont-cp.png", "script/eda-figures.R",
    46L, "figure", "deliv/figure/eda/conc-time-s1.png", "script/eda-figures.R",
    47L, "figure", "deliv/figure/eda/conc-time-s2.png", "script/eda-figures.R",
    48L, "figure", "deliv/figure/eda/dnconc-time-s2.png", "script/eda-figures.R",
    49L, "figure", "deliv/figure/eda/conc-time-s3.png", "script/eda-figures.R",
    50L, "figure", "deliv/figure/eda/conc-time-s4.png", "script/eda-figures.R",
    51L, "figure", "deliv/figure/102/102-dv-pred-ipred.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    52L, "figure", "deliv/figure/102/102-dv-pred-by-rf.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    53L, "figure", "deliv/figure/102/102-dv-ipred-by-rf.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    54L, "figure", "deliv/figure/102/102-dv-pred-by-cp.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    55L, "figure", "deliv/figure/102/102-dv-ipred-by-cp.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    56L, "figure", "deliv/figure/102/102-cwres-combined.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    57L, "figure", "deliv/figure/102/102-npde-combined.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    58L, "figure", "deliv/figure/102/102-npde-cont-cov.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    59L, "figure", "deliv/figure/102/102-npde-cat-cov.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    60L, "figure", "deliv/figure/102/102-eta-pairs.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    61L, "figure", "deliv/figure/102/102-eta-all-cont-cov.pdf page: 1", "script/diagnostic-templates/diagnostics-report.Rmd",
    62L, "figure", "deliv/figure/102/102-eta-all-cont-cov.pdf page: 2", "script/diagnostic-templates/diagnostics-report.Rmd",
    63L, "figure", "deliv/figure/102/102-eta-all-cont-cov.pdf page: 3", "script/diagnostic-templates/diagnostics-report.Rmd",
    64L, "figure", "deliv/figure/102/102-eta-all-cat-cov.pdf page: 2", "script/diagnostic-templates/diagnostics-report.Rmd",
    65L, "figure", "deliv/figure/102/102-eta-all-cat-cov.pdf page: 3", "script/diagnostic-templates/diagnostics-report.Rmd",
    66L, "figure", "deliv/figure/106/106-dv-pred-ipred.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    67L, "figure", "deliv/figure/106/106-dv-pred-by-rf.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    68L, "figure", "deliv/figure/106/106-dv-ipred-by-rf.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    69L, "figure", "deliv/figure/106/106-dv-pred-by-cp.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    70L, "figure", "deliv/figure/106/106-dv-ipred-by-cp.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    71L, "figure", "deliv/figure/106/106-cwres-combined.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    72L, "figure", "deliv/figure/106/106-npde-combined.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    73L, "figure", "deliv/figure/106/106-npde-cont-cov.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    74L, "figure", "deliv/figure/106/106-npde-cat-cov.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    75L, "figure", "deliv/figure/106/106-eta-pairs.pdf", "script/diagnostic-templates/diagnostics-report.Rmd",
    76L, "figure", "deliv/figure/106/106-eta-all-cont-cov.pdf page: 1", "script/diagnostic-templates/diagnostics-report.Rmd",
    77L, "figure", "deliv/figure/106/106-eta-all-cont-cov.pdf page: 2", "script/diagnostic-templates/diagnostics-report.Rmd",
    78L, "figure", "deliv/figure/106/106-eta-all-cont-cov.pdf page: 3", "script/diagnostic-templates/diagnostics-report.Rmd",
    79L, "figure", "deliv/figure/106/106-eta-all-cat-cov.pdf page: 2", "script/diagnostic-templates/diagnostics-report.Rmd",
    80L, "figure", "deliv/figure/106/106-eta-all-cat-cov.pdf page: 3", "script/diagnostic-templates/diagnostics-report.Rmd",
    81L, "figure", "deliv/figure/106/106-covplot-cl.png", "script/forest-plots.R",
    82L, "figure", "deliv/figure/106/106-pk-sim-renal-normalized-auc-norm-range.png", "script/pk-sim-renal.R",
    83L, "figure", "deliv/figure/106/106-pk-sim-renal-auc-raw-range.png", "script/pk-sim-renal.R",
    84L, "figure", "deliv/figure/pk-vpc-106-dose-norm.png", "script/pk-vpc-final.R",
    85L, "figure", "deliv/figure/pk-vpc-106-dose-norm-rf.png", "script/pk-vpc-final.R",
    86L, "figure", "deliv/figure/pk-vpc-106-dose-norm-cp.png", "script/pk-vpc-final.R",
    87L, "figure", "deliv/figure/pk-pcheck-final-auc-pcheck.png", "pk-pcheck-final.R",
    88L, "figure", "deliv/figure/pk-pcheck-final-cmin-pcheck.png", "pk-pcheck-final.R",
    91L, "table", "metworx-software.tex", "script/metworx-software.R"
  )

  result <- getTFs(report)
  # No TeX file for this fixture, so all rows are in pdf_only
  actual_tfs <- dplyr::bind_rows(result$pdf_and_tex, result$pdf_only)
  actual_tfs <- actual_tfs[order(actual_tfs$page), ]
  rownames(actual_tfs) <- NULL

  testthat::expect_equal(
    actual_tfs[names(expected_tfs)],
    as.data.frame(expected_tfs)
  )
  testthat::expect_equal(nrow(result$tex_only), 0L)
  testthat::expect_true(any(actual_tfs$source_code_exists))
})

test_that("pdf_image_pages lists pages that contain a raster image", {
  report <- file.path("fixtures", "figures", "report.pdf")
  if (!file.exists(report)) {
    report <- file.path("tests", "testthat", "fixtures", "figures", "report.pdf")
  }
  testthat::skip_if(!nzchar(Sys.which("pdfimages")), "pdfimages CLI not available")

  image_pages <- review:::pdf_image_pages(report)

  testthat::expect_true(is.integer(image_pages))
  testthat::expect_false(is.unsorted(image_pages))
  # Figure pages carrying embedded source captions must be detected as images.
  testthat::expect_true(all(c(43L, 87L, 88L) %in% image_pages))
})

report_tex_fixture_report <- function() {
  testthat::test_path("fixtures", "report-tex", "deliv", "report", "report.pdf")
}

report_tex_expected_path <- function(report, path) {
  fixture_root <- testthat::test_path("fixtures", "report-tex")
  review:::source_relative_path(
    normalizePath(file.path(fixture_root, path), winslash = "/", mustWork = FALSE),
    review:::report_path_roots()
  )
}

test_that("standalone TeX path extractors return stable relative paths", {
  report <- report_tex_fixture_report()
  figures_tex <- file.path(dirname(report), "sections", "figures.tex")
  tables_tex <- file.path(dirname(report), "sections", "tables.tex")

  figure_paths <- getFigurePathsFromTex(figures_tex)
  table_paths <- getTablePathsFromTex(tables_tex)

  testthat::expect_equal(
    figure_paths,
    c(
      report_tex_expected_path(report, "deliv/figure/eda/cont-vs-cont.png"),
      report_tex_expected_path(report, "deliv/figure/pk-vpc-106-dose-norm.png"),
      report_tex_expected_path(report, "deliv/figure/106/106-cwres-combined.pdf")
    )
  )
  testthat::expect_equal(
    table_paths,
    c(
      report_tex_expected_path(report, "deliv/table/report/pk-data-sum.tex"),
      report_tex_expected_path(report, "deliv/table/report/base-param-fixed.tex")
    )
  )
})

test_that("report TeX paths are discovered next to the PDF", {
  report <- report_tex_fixture_report()

  paths <- report_tex_paths(report, .figures_tex = "auto", .tables_tex = "auto")

  testthat::expect_true(any(paths$type == "figure"))
  testthat::expect_true(any(paths$type == "table"))
  testthat::expect_true(
    report_tex_expected_path(report, "deliv/figure/pk-vpc-106-dose-norm.png") %in%
      paths$path
  )
  testthat::expect_true(
    report_tex_expected_path(report, "deliv/figure/106/106-cwres-combined.pdf") %in%
      paths$path
  )
  testthat::expect_true(
    report_tex_expected_path(report, "deliv/table/report/base-param-fixed.tex") %in%
      paths$path
  )
  testthat::expect_false(
    report_tex_expected_path(report, "deliv/figure/commented-out.png") %in% paths$path
  )
})

test_that("report TeX paths can correct close extracted artifact paths", {
  tex_paths <- tibble::tibble(
    type = c("figure", "table"),
    path = c(
      "deliv/figure/pk-vpc-106-dose-norm.png",
      "deliv/table/report/base-param-fixed.tex"
    )
  )

  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/figure/pk-vpc-106-dose-noem.png",
      "figure",
      tex_paths
    ),
    "deliv/figure/pk-vpc-106-dose-norm.png"
  )
  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/table/report/base-param-fxed.tex",
      "table",
      tex_paths
    ),
    "deliv/table/report/base-param-fixed.tex"
  )
  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/figure/106/106-cwres-combimed.pdf",
      "figure",
      tibble::tibble(
        type = "figure",
        path = "deliv/figure/106/106-cwres-combined.pdf"
      )
    ),
    "deliv/figure/106/106-cwres-combined.pdf"
  )

  # page: N suffix with N > 1 maps to -pageN filename variant
  tex_paths_paged <- tibble::tibble(
    type = "figure",
    path = c(
      "deliv/figure/102/102-eta-all-cont-cov.png",
      "deliv/figure/102/102-eta-all-cont-cov-page2.png",
      "deliv/figure/102/102-eta-all-cont-cov-page3.png"
    )
  )
  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/figure/102/102-eta-all-cont-cov.pdf page: 1",
      "figure",
      tex_paths_paged
    ),
    "deliv/figure/102/102-eta-all-cont-cov.png page: 1"
  )
  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/figure/102/102-eta-all-cont-cov.pdf page: 2",
      "figure",
      tex_paths_paged
    ),
    "deliv/figure/102/102-eta-all-cont-cov-page2.png"
  )
  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/figure/102/102-eta-all-cont-cov.pdf page: 3",
      "figure",
      tex_paths_paged
    ),
    "deliv/figure/102/102-eta-all-cont-cov-page3.png"
  )
  # cat-cov has no page: 1 entry in TeX, so page: 2 and page: 3 should
  # still resolve to -page2 / -page3 variants
  tex_paths_cat <- tibble::tibble(
    type = "figure",
    path = c(
      "deliv/figure/102/102-eta-all-cat-cov-page2.png",
      "deliv/figure/102/102-eta-all-cat-cov-page3.png"
    )
  )
  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/figure/102/102-eta-all-cat-cov.pdf page: 2",
      "figure",
      tex_paths_cat
    ),
    "deliv/figure/102/102-eta-all-cat-cov-page2.png"
  )
  testthat::expect_equal(
    correct_report_tex_path(
      "deliv/figure/102/102-eta-all-cat-cov.pdf page: 3",
      "figure",
      tex_paths_cat
    ),
    "deliv/figure/102/102-eta-all-cat-cov-page3.png"
  )
})

test_that("TeX-only report paths are returned as non-PDF rows", {
  report <- report_tex_fixture_report()
  figure_path <- report_tex_expected_path(report, "deliv/figure/pk-vpc-106-dose-norm.png")
  table_path <- report_tex_expected_path(report, "deliv/table/report/base-param-fixed.tex")

  df <- tibble::tibble(
    page = 1L,
    type = "figure",
    source_graphic = figure_path,
    source_code = "script/pk-vpc-final.R"
  )
  tex_paths <- tibble::tibble(
    type = c("figure", "table"),
    path = c(
      figure_path,
      table_path
    )
  )

  df <- add_tf_diagnostics(df, report, tex_paths, "tests/testthat/fixtures")
  tex_only <- build_tex_only_rows(df, tex_paths)

  testthat::expect_equal(nrow(tex_only), 1L)
  testthat::expect_equal(tex_only$source_graphic, table_path)
  testthat::expect_true("source_graphic_exists" %in% names(tex_only))
  testthat::expect_false("source_code" %in% names(tex_only))
})
