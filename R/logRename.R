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
  root <- logRoot()
  old <- pathFromLogRoot(.filepath)
  new <- pathFromLogRoot(.new_filepath)
  qclog <- logRead(root)
  changed <- qclog$file %in% old | qclog$origin %in% old

  if (!any(changed)) {
    cli::cli_abort("{.path {old}} does not exist in QClog.csv")
  }

  old_path <- fs::path(root, old)
  new_path <- fs::path(root, new)
  if (!fs::file_exists(old_path)) {
    cli::cli_abort("File does not exist: {.path {old}}")
  }
  if (fs::file_exists(new_path)) {
    cli::cli_abort("New file path already exists: {.path {new}}")
  }
  if (!fs::dir_exists(fs::path_dir(new_path))) {
    cli::cli_abort(
      "New file parent directory does not exist: {.path {fs::path_dir(new)}}"
    )
  }

  svnRun("mv", old_path, new_path)

  qclog$file[qclog$file %in% old] <- new
  qclog$origin[qclog$origin %in% old] <- new
  logWrite(qclog, file = logName(root))

  cli::cli_alert_info("Renamed '{old}' to '{new}' in SVN and QClog.csv")

  invisible(sum(changed))
}
