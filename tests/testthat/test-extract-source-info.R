expected_source_info <- list(
  "pk-pcheck-final-auc-pcheck.png" = list(
    Source_Code = "pk-pcheck-final.R",
    Source_Graphic = "deliv/figure/pk-pcheck-final-auc-pcheck.png"
  ),
  "cat-vs-cont-rf.png" = list(
    Source_Code = "script/eda-figures.R",
    Source_Graphic = "deliv/figure/eda/cat-vs-cont-rf.png"
  ),
  "conc-time-s2.png" = list(
    Source_Code = "script/eda-figures.R",
    Source_Graphic = "deliv/figure/eda/conc-time-s2.png"
  ),
  "cont-vs-cont.png" = list(
    Source_Code = "eda-figures.R",
    Source_Graphic = "deliv/figure/report/cont-vs-cont.png"
  ),
  "id-dose-dv022.png" = list(
    Source_Code = "eda-figures.R",
    Source_Graphic = "deliv/figure/report/id-dose-dv022.png"
  ),
  "pk-npde-final-vs-pred-by-rf.png" = list(
    Source_Code = "pk-npde-final.R",
    Source_Graphic = "deliv/figure/pk-npde-final-vs-pred-by-rf.png"
  ),
  "pk-npde-final-vs-time.png" = list(
    Source_Code = "pk-npde-final.R",
    Source_Graphic = "deliv/figure/pk-npde-final-vs-time.png"
  ),
  "pk-plots001.png" = list(
    Source_Code = "eda-figures.R",
    Source_Graphic = "deliv/figure/report/pk-plots001.png"
  ),
  "pk-sim-renal-enrolled-disease-distribution.png" = list(
    Source_Code = "pk-sim-renal.R",
    Source_Graphic = "pk-sim-renal-enrolled-disease-distribution.png"
  ),
  "pk-sim-renal-normalized-auc-norm-range.png" = list(
    Source_Code = "pk-sim-renal.R",
    Source_Graphic = "deliv/figure/pk-sim-renal-normalized-auc-norm-range.png"
  ),
  "pk-vpc-106-BLQ.png" = list(
    Source_Code = "script/pk-vpc-final.R",
    Source_Graphic = "deliv/figure/pk-vpc-106-BLQ.png"
  ),
  "pk-vpc-106-dose-norm.png" = list(
    Source_Code = "script/pk-vpc-final.R",
    Source_Graphic = "deliv/figure/pk-vpc-106-dose-norm.png"
  ),
  "pk-vpc-106-pred-corr.png" = list(
    Source_Code = "pk-pcvpc-final.R",
    Source_Graphic = "deliv/figure/pk-vpc-106-pred-corr.png"
  )
)

figure_path <- function(file) {
  source_paths <- file.path(
    c(
      "fixtures/figures",
      "tests/testthat/fixtures/figures",
      "../../tests/testthat/fixtures/figures"
    ),
    file
  )
  source_paths[file.exists(source_paths)][1]
}

test_that("extract_source_info reads source metadata from PNG figures", {
  paths <- vapply(names(expected_source_info), figure_path, character(1))
  testthat::expect_true(
    all(file.exists(paths)),
    info = paste(
      "Missing source figure fixtures:",
      paste(names(paths)[!file.exists(paths)], collapse = ", ")
    )
  )

  actual_source_info <- lapply(paths, extract_source_info)
  names(actual_source_info) <- names(expected_source_info)

  testthat::expect_equal(actual_source_info, expected_source_info)
})

test_that("source code candidate selection prefers files that exist on disk", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "script"))
  file.create(file.path(root, "script", "pk-vpc-final.R"))

  candidates <- tibble::tibble(
    Candidate_Type = c("source_block", "source_block"),
    Source_Code = c("script/pk-vpce-final.R", "script/pk-vpc-final.R"),
    Source_Code_Confidence = c(95, 45),
    Source_Graphic = NA_character_,
    Source_Graphic_Confidence = NA_real_
  )

  testthat::expect_equal(
    choose_source_info_candidate(
      candidates,
      "Source_Code",
      source_root = root
    ),
    "script/pk-vpc-final.R"
  )
})

test_that("source code candidate selection corrects close OCR spellings", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "script"))
  file.create(file.path(root, "script", "pk-vpc-final.R"))

  candidates <- tibble::tibble(
    Candidate_Type = "source_block",
    Source_Code = "script/pk-vpce-final.R",
    Source_Code_Confidence = 95,
    Source_Graphic = NA_character_,
    Source_Graphic_Confidence = NA_real_
  )

  testthat::expect_equal(
    choose_source_info_candidate(
      candidates,
      "Source_Code",
      source_root = root
    ),
    "script/pk-vpc-final.R"
  )
})

test_that("source code candidate selection rejects malformed paths", {
  candidates <- tibble::tibble(
    Candidate_Type = "source_block",
    Source_Code = "script/pk-vpc-final",
    Source_Code_Confidence = 95,
    Source_Graphic = NA_character_,
    Source_Graphic_Confidence = NA_real_
  )

  testthat::expect_true(is.na(
    choose_source_info_candidate(candidates, "Source_Code")
  ))
})

test_that("a graphic is confirmed only by a close match to a TeX figure path", {
  tex <- c("deliv/figure/pk-vpc-106-dose-norm.png", "deliv/figure/eda/cont-vs-cont.png")

  # Exact and close OCR reads both resolve to a known TeX path.
  testthat::expect_true(
    source_graphic_confirmed("deliv/figure/pk-vpc-106-dose-norm.png", tex)
  )
  testthat::expect_true(
    source_graphic_confirmed("deliv/figure/pk-vpc-106-dose-noem.png", tex)
  )
  # No TeX, missing value, or a path absent from TeX cannot be confirmed.
  testthat::expect_false(
    source_graphic_confirmed("deliv/figure/pk-vpc-106-dose-norm.png", character())
  )
  testthat::expect_false(source_graphic_confirmed(NA_character_, tex))
  testthat::expect_false(
    source_graphic_confirmed("deliv/figure/not-in-tex.png", tex)
  )
})

test_that("source code is confirmed only when it resolves to a file on disk", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "script"))
  file.create(file.path(root, "script", "pk-vpc-final.R"))

  testthat::expect_true(source_code_confirmed("script/pk-vpc-final.R", root))
  # Fuzzy/typo reads do not resolve exactly, so they keep escalating.
  testthat::expect_false(source_code_confirmed("script/pk-vpce-final.R", root))
  # No roots, missing value, or malformed path cannot be confirmed.
  testthat::expect_false(source_code_confirmed("script/pk-vpc-final.R", NULL))
  testthat::expect_false(source_code_confirmed(NA_character_, root))
  testthat::expect_false(source_code_confirmed("script/pk-vpc-final", root))
})
