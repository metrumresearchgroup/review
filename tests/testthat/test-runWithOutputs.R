test_that("readOutputs returns empty tibble when outputs directory is missing", {
  outputs_dir <- here::here("data", "outputs")
  backup_dir <- NULL

  if (fs::dir_exists(outputs_dir)) {
    backup_dir <- tempfile("outputs-backup-", tmpdir = here::here("data"))
    fs::file_move(outputs_dir, backup_dir)
  }

  withr::defer({
    if (fs::dir_exists(outputs_dir)) {
      fs::dir_delete(outputs_dir)
    }
    if (!is.null(backup_dir) && fs::dir_exists(backup_dir)) {
      fs::file_move(backup_dir, outputs_dir)
    }
  })

  expect_equal(readOutputs(), dplyr::tibble())
})


test_that("readOutputs returns empty tibble when no CSV files are present", {
  outputs_dir <- here::here("data", "outputs")
  backup_dir <- NULL

  if (fs::dir_exists(outputs_dir)) {
    backup_dir <- tempfile("outputs-backup-", tmpdir = here::here("data"))
    fs::file_move(outputs_dir, backup_dir)
  }

  withr::defer({
    if (fs::dir_exists(outputs_dir)) {
      fs::dir_delete(outputs_dir)
    }
    if (!is.null(backup_dir) && fs::dir_exists(backup_dir)) {
      fs::file_move(backup_dir, outputs_dir)
    }
  })

  fs::dir_create(outputs_dir, recurse = TRUE)

  expect_equal(readOutputs(), dplyr::tibble())
})


test_that("runWithOutputs captures console logs and file events", {
  fixture_dir <- here::here("data", "runwithoutputs-test")
  fs::dir_create(fixture_dir, recurse = TRUE)
  withr::defer({
    if (fs::dir_exists(fixture_dir)) {
      fs::dir_delete(fixture_dir)
    }
  })

  modify_path <- fs::path(fixture_dir, "modify-me.txt")
  delete_path <- fs::path(fixture_dir, "delete-me.txt")
  readr::write_lines("original", modify_path)
  readr::write_lines("delete me", delete_path)
  Sys.setFileTime(modify_path, Sys.time() - 10)

  script_dir <- here::here("tests", "testthat", "tmp-runWithOutputs")
  fs::dir_create(script_dir, recurse = TRUE)
  withr::defer({
    if (fs::dir_exists(script_dir)) {
      fs::dir_delete(script_dir)
    }
  })

  script_path <- fs::path(script_dir, "sample-script.R")
  script_lines <- c(
    "cat('script starting\\n')",
    "output_dir <- 'data/runwithoutputs-test'",
    "dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)",
    "writeLines('created content', file.path(output_dir, 'created.txt'))",
    "writeLines('updated content', file.path(output_dir, 'modify-me.txt'))",
    "unlink(file.path(output_dir, 'delete-me.txt'))"
  )
  writeLines(script_lines, script_path)

  script_abs <- fs::path_abs(script_path)
  script_rel <- fs::path_rel(script_abs, start = here::here())
  script_tag <- paste(
    fs::path_split(fs::path_ext_remove(script_rel))[[1]],
    collapse = "--"
  )
  script_chr <- as.character(script_rel)

  log_path <- here::here("data", "outputs", paste0(script_tag, ".log"))
  csv_path <- here::here("data", "outputs", paste0(script_tag, "-outputs.csv"))

  withr::defer({
    to_clean <- c(log_path, csv_path)
    to_clean <- to_clean[fs::file_exists(to_clean)]
    if (length(to_clean)) {
      fs::file_delete(to_clean)
    }
  })

  runWithOutputs(script_abs)

  expect_true(fs::file_exists(log_path))
  log_contents <- readLines(log_path)
  expect_true(any(grepl("script starting", log_contents, fixed = TRUE)))

  expect_true(fs::file_exists(csv_path))
  outputs <- readr::read_csv(csv_path, show_col_types = FALSE)
  outputs <- dplyr::arrange(outputs, event, output)

  expected <- dplyr::tibble(
    script = rep(script_chr, 3),
    event = c("created", "deleted", "modified"),
    output = c(
      "data/runwithoutputs-test/created.txt",
      "data/runwithoutputs-test/delete-me.txt",
      "data/runwithoutputs-test/modify-me.txt"
    )
  )
  expected <- dplyr::arrange(expected, event, output)

  expect_equal(outputs, expected)

  aggregated <- readOutputs()
  aggregated_filtered <- dplyr::arrange(
    dplyr::filter(aggregated, script == script_chr),
    event,
    output
  )

  expect_equal(aggregated_filtered, expected)
})


test_that("runWithOutputs errors when the script is missing", {
  expect_error(
    runWithOutputs("tests/testthat/no-such-script.R"),
    "Script not found"
  )
})
