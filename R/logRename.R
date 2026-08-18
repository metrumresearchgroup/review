#' Rename a file in the QC log and SVN
#'
#' @description
#' Renames a version-controlled file with `svn mv` and updates every reference
#' to its path in the `file` and `origin` columns of `QClog.csv`.
#'
#' Paths are resolved relative to the root of the QC log, including when the
#' function is called from a subdirectory.
#'
#' @param .filepath Current path of the file to rename.
#' @param .new_filepath New path for the file.
#'
#' @return Invisibly returns the number of rows changed in `QClog.csv`.
#'
#' @examples
#' \dontrun{
#' logRename("script/old-name.R", "script/new-name.R")
#' }
#'
#' @export
logRename <- function(.filepath, .new_filepath) {
  is_path <- function(path) {
    is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
  }

  if (!is_path(.filepath) || !is_path(.new_filepath)) {
    cli::cli_abort(
      "{.arg .filepath} and {.arg .new_filepath} must each be a non-empty path"
    )
  }

  root <- logRoot()
  current <- pathFromLogRoot(.filepath)
  new <- pathFromLogRoot(.new_filepath)

  if (identical(current, new)) {
    cli::cli_abort("The current and new file paths must be different")
  }

  qclog <- logRead(root)
  changed <- qclog$file %in% current | qclog$origin %in% current
  if (!any(changed)) {
    cli::cli_abort("{.path {current}} does not exist in QClog.csv")
  }

  current_absolute <- file.path(root, current)
  new_absolute <- file.path(root, new)

  if (!file.exists(current_absolute)) {
    cli::cli_abort("File does not exist: {.path {current}}")
  }
  if (file.exists(new_absolute)) {
    cli::cli_abort("New file path already exists: {.path {new}}")
  }
  if (!dir.exists(dirname(new_absolute))) {
    cli::cli_abort(
      "New file parent directory does not exist: {.path {dirname(new)}}"
    )
  }

  svnCommand(
    .command = "mv",
    # svnCommand adds the outer quotes; these quotes separate the two paths.
    .file = paste0(current_absolute, "' '", new_absolute),
    .xml = FALSE
  )

  qclog$file[qclog$file %in% current] <- new
  qclog$origin[qclog$origin %in% current] <- new
  logWrite(qclog, file = logName(root))

  cli::cli_alert_info(
    glue::glue("Renamed '{current}' to '{new}' in SVN and QClog.csv")
  )

  invisible(sum(changed))
}
