#' Run a Script and Report Outputs
#'
#' Runs an R script from the project root in a clean R session and reports any
#' files saved (created or updated) during the run.
#'
#' @param script Path to the script to execute.
#' @param root Path to the project root. Defaults to `here::here()`.
#' @param exclude_dirs Character vector of directories relative to `root` to ignore.
#'
#' @return Invisibly returns a vector of relative paths for changed files.
#' @export
runWithOutputs <- function(
  script,
  root = here::here(),
  exclude_dirs = c("renv", ".svn", ".git")
) {
  # Normalize script path to absolute so callr finds it regardless of 'wd' change
  script_abs <- fs::path_abs(script)

  # Snapshot before
  # We track size as well to catch changes that might happen within the same second
  # on low-resolution file systems if the content length changes.
  before <- fs::dir_info(root, recurse = TRUE, type = "file") %>%
    dplyr::select(path, modification_time, size)

  # --- Start Header ---
  div_start <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule(
    left = cli::style_bold(cli::bg_cyan(cli::col_white(" runWithOutputs() "))),
    right = "START"
  )
  cli::cli_bullets(c(
    "*" = paste0(cli::style_bold("Script: "), cli::col_blue("{.path {script}}"))
  ))
  cli::cli_end(div_start)

  # Execute script
  # We use callr::rscript to run in a fresh session
  callr::rscript(script_abs, wd = root, show = TRUE)

  # --- End Header ---
  div_end <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule(
    left = cli::style_bold(cli::bg_cyan(cli::col_white(" runWithOutputs() "))),
    right = "COMPLETE"
  )
  cli::cli_end(div_end)

  # Snapshot after
  after <- fs::dir_info(root, recurse = TRUE, type = "file") %>%
    dplyr::select(path, modification_time, size)

  # Detect changes
  changed <- dplyr::anti_join(
    after,
    before,
    by = c("path", "modification_time", "size")
  ) %>%
    dplyr::mutate(rel_path = fs::path_rel(path, start = root))

  # Filter exclusions using regex
  if (length(exclude_dirs) > 0) {
    safe_dirs <- gsub(".", "\\.", exclude_dirs, fixed = TRUE)
    # Ensure we match directories at the start of the relative path
    pattern <- paste0("^(", paste(safe_dirs, collapse = "|"), ")/")
    changed <- dplyr::filter(changed, !grepl(pattern, rel_path))
  }

  # Output
  out_paths <- sort(changed$rel_path)

  if (length(out_paths) > 0) {
    cli::cli_alert_success("Files saved by this run:")
    cli::cli_code(yaml::as.yaml(list(outputs = out_paths)))
    invisible(out_paths)
  } else {
    cli::cli_alert_success(cli::col_silver("No files were saved."))
    invisible(character(0))
  }
}
