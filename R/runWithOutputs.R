#' Execute Script and Detect File Changes
#'
#' Runs an R script in an isolated session and identifies files that were created
#' or modified during execution. It detects changes by comparing the file system
#' state (modification time and size) before and after the run.
#'
#' @param script Character. The path to the R script to execute.
#' @param root Character. The project root directory. Defaults to `here::here()`.
#' @param exclude_dirs Character vector. A list of top-level directories relative
#'   to `root` to ignore when scanning for changes (e.g., "renv", ".git").
#'
#' @return Invisibly returns a character vector of relative paths for all files
#'   that were created or updated.
#' @export
runWithOutputs <- function(
  script,
  root = here::here(),
  exclude_dirs = c("renv", ".svn", ".git")
) {
  # Normalize paths and calculate relative path for the UI
  script_abs <- fs::path_abs(script)
  script_rel <- fs::path_rel(script_abs, start = root)

  # 1. Capture Initial State
  # Define file state by path, modification time, and size.
  before <- fs::dir_info(root, recurse = TRUE, type = "file") %>%
    dplyr::select(path, modification_time, size)

  # --- UI Header ---
  # Shows the relative path in the badge for immediate context.
  div_start <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule(
    left = cli::style_bold(cli::bg_cyan(cli::col_white(paste0(
      " runWithOutputs('",
      script_rel,
      "') "
    )))),
    right = "START"
  )
  cli::cli_end(div_start)

  # 2. Execute Script
  # Run in a clean, separate R session to ensure isolation.
  callr::rscript(script_abs, wd = root, show = TRUE)

  # --- UI Footer ---
  # Provides a clean "closing bracket" for the script output.
  div_end <- cli::cli_div(theme = list(rule = list(color = "cyan")))
  cli::cli_rule(
    left = cli::style_bold(cli::bg_cyan(cli::col_white(" runWithOutputs() "))),
    right = "COMPLETE"
  )
  cli::cli_end(div_end)

  # 3. Capture Final State
  after <- fs::dir_info(root, recurse = TRUE, type = "file") %>%
    dplyr::select(path, modification_time, size)

  # 4. Compute State Differences
  # Identify files where the (path, time, size) tuple in the 'after' snapshot
  # does not strictly match the 'before' snapshot.
  changed <- dplyr::anti_join(
    after,
    before,
    by = c("path", "modification_time", "size")
  ) %>%
    dplyr::mutate(rel_path = fs::path_rel(path, start = root))

  # 5. Apply Exclusions
  # Filter out files where the top-level directory matches an exclusion pattern.
  if (length(exclude_dirs) > 0 && nrow(changed) > 0) {
    changed <- changed %>%
      dplyr::filter(
        !fs::path_split(rel_path) %>%
          purrr::map_lgl(~ .x[1] %in% exclude_dirs)
      )
  }

  # 6. Report Results
  out_paths <- sort(changed$rel_path)

  if (length(out_paths) > 0) {
    cli::cli_alert_success("Files saved by this run:")
    cli::cli_code(yaml::as.yaml(list(outputs = out_paths)))
    invisible(out_paths)
  } else {
    cli::cli_alert_info(cli::col_silver("No files were saved."))
    invisible(character(0))
  }
}
