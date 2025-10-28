#' Run an R script and capture the generated outputs
#'
#' @description
#' Executes an R script in a separate R session using `here::here()` as the
#' working directory. Console output is written to
#' `data/outputs/<script-path>.log`, where `<script-path>` is the script's path
#' relative to the project root (without the file extension) with folder levels
#' separated by `--`. File creation, deletion, and modification events are
#' recorded in `data/outputs/<script-path>-outputs.csv`. Each CSV row contains
#' the event type and the affected output path relative to the project root.
#'
#' @param script Path to the R script to execute. Can be relative to the project
#'   root or the current working directory.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' runWithOutputs("inst/scripts/report.R")
#' }
#' @export
runWithOutputs <- function(script) {
  wd <- here::here()
  # First interpret `script` relative to the caller's current working directory
  # (supports callers who setwd() into a subfolder), then fall back to treating it
  # as project-root relative. Deduplicate so we don't double-check the same path.
  candidates <- unique(c(
    fs::path_abs(script),
    fs::path_abs(script, start = wd)
  ))
  existing <- candidates[fs::file_exists(candidates)]
  if (length(existing) == 0L) {
    cli::cli_abort("Script not found: {.file {script}}")
  }

  script_abs <- existing[[1]]

  script_rel <- fs::path_rel(script_abs, start = wd)
  script_rel_no_ext <- fs::path_ext_remove(script_rel)
  script_tag <- paste(fs::path_split(script_rel_no_ext)[[1]], collapse = "--")

  # Keep everything the script produces inside data/outputs so people always
  # know where to look; the flattened tag gives each script its own log without
  # recreating the full folder path.
  output_dir <- fs::path(wd, "data", "outputs")
  log_path <- fs::path(output_dir, paste0(script_tag, ".log"))
  csv_path <- fs::path(output_dir, paste0(script_tag, "-outputs.csv"))
  csv_rel <- fs::path_rel(csv_path, start = wd)
  log_rel <- fs::path_rel(log_path, start = wd)
  fs::dir_create(output_dir, recurse = TRUE)
  if (fs::file_exists(log_path)) {
    fs::file_delete(log_path)
  }
  if (fs::file_exists(csv_path)) {
    fs::file_delete(csv_path)
  }

  snapshot <- function(root) {
    # Skip renv internals and files we already recorded from earlier runs; they
    # don't matter to the user and including them would bury the real changes.
    paths <- fs::dir_ls(
      path = root,
      recurse = TRUE,
      type = "file",
      regexp = "(/renv/|/data/outputs/)",
      invert = TRUE
    )
    info <- fs::file_info(paths)
    dplyr::tibble(
      path = fs::path_rel(paths, start = root),
      mtime = info$modification_time
    )
  }

  # Take a snapshot of the project before the run so we can compare afterward
  # and know exactly which files the script touched.
  pre <- snapshot(wd)

  # Send all console output into one temporary file so this R session stays
  # quiet and we can show the transcript once the script finishes.
  stdout_tmp <- fs::file_temp(
    pattern = paste0(script_tag, "_console_"),
    ext = ".log"
  )
  on.exit(
    if (fs::file_exists(stdout_tmp)) fs::file_delete(stdout_tmp),
    add = TRUE
  )

  run_id <- cli::cli_process_start(
    "Running in a fresh R session: {.file {script_rel}}"
  )

  exit_status <- tryCatch(
    {
      # Start a fresh R session so the script can't interfere with our current
      # work or rely on hidden state; turn off colours so the saved log looks the
      # same for everyone.
      callr::rscript(
        script = script_abs,
        wd = wd,
        user_profile = TRUE,
        system_profile = FALSE,
        cmdargs = c("--no-save", "--no-restore"),
        stdout = stdout_tmp,
        stderr = stdout_tmp,
        show = FALSE,
        spinner = TRUE,
        env = c(
          CLI_NUM_COLORS = "0",
          R_CLI_NUM_COLORS = "0",
          NO_COLOR = "1",
          CRAYON_ENABLED = "FALSE",
          CLICOLOR_FORCE = "0"
        )
      )
      0L
    },
    error = function(e) 1L
  )

  if (!fs::file_exists(stdout_tmp)) {
    fs::file_create(stdout_tmp)
  }
  # Copy the transcript into the permanent log even when the script fails so
  # there's always something to read when debugging.
  fs::file_copy(stdout_tmp, log_path, overwrite = TRUE)

  if (exit_status == 0L) {
    cli::cli_process_done(run_id)
  } else {
    cli::cli_process_failed(run_id)
    cli::cli_abort(
      "Script failed. See log for details: {.file {log_rel}}",
      call = NULL
    )
  }

  post <- snapshot(wd)

  # Compare the before-and-after snapshots so we can tag each file as created,
  # deleted, or changed without crawling the whole disk again.
  changes <- dplyr::full_join(
    pre,
    post,
    by = "path",
    suffix = c("_pre", "_post")
  )

  with_events <- changes %>%
    dplyr::mutate(
      event = dplyr::case_when(
        is.na(mtime_pre) & !is.na(mtime_post) ~ "created",
        !is.na(mtime_pre) & is.na(mtime_post) ~ "deleted",
        !is.na(mtime_pre) & !is.na(mtime_post) & mtime_pre != mtime_post ~
          "modified",
        TRUE ~ NA_character_
      )
    )

  files_df <- with_events %>%
    dplyr::filter(!is.na(event)) %>%
    dplyr::transmute(script = script_rel, event, output = path)

  # Always write the CSV, even if it's empty, so callers don't have to check for
  # the file before reading it.
  readr::write_csv(files_df, csv_path)
  cli::cli_alert_success("Outputs written: {.file {csv_rel}}")

  invisible(NULL)
}

#' Read outputs generated by `runWithOutputs()`
#'
#' @description
#' Aggregates the per-script CSV files generated by `runWithOutputs()` into a
#' single tibble. Each row records the event type (`created`, `deleted`,
#' `modified`) and the relative path of an affected output file, grouped by the
#' script that produced the change.
#'
#' @param dir Directory that contains the CSV files written by
#'   `runWithOutputs()`.
#'
#' @return A tibble combining the CSV contents. Returns an empty tibble when no
#'   CSV files are present.
#'
#' @examples
#' \dontrun{
#' readOutputs()
#' }
#' @export
readOutputs <- function(dir = here::here("data", "outputs")) {
  if (!fs::dir_exists(dir)) {
    return(dplyr::tibble())
  }

  files <- fs::dir_ls(dir, type = "file", glob = "*.csv")
  if (length(files) == 0L) {
    return(dplyr::tibble())
  }

  # Sort the file list ourselves because different machines return glob matches
  # in different orders.
  files <- sort(as.character(files))
  names(files) <- fs::path_file(files)

  purrr::map_dfr(
    files,
    ~ readr::read_csv(.x, show_col_types = FALSE),
    .id = "file"
  ) %>%
    dplyr::transmute(file = script, event, output)
}
