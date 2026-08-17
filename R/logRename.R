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
  paths <- list(current = .filepath, new = .new_filepath)
  valid_path <- purrr::map_lgl(
    paths,
    function(path) {
      is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
    }
  )

  if (!all(valid_path)) {
    stop("`.filepath` and `.new_filepath` must each be a non-empty path")
  }

  root <- logRoot()
  qclog <- logRead(root)
  current <- pathFromLogRoot(.filepath)
  new <- pathFromLogRoot(.new_filepath)

  if (identical(current, new)) {
    stop("The current and new file paths must be different")
  }

  changed <- qclog$file %in% current | qclog$origin %in% current
  if (!any(changed)) {
    stop(paste0(current, " does not exist in QClog.csv"))
  }

  absolute_path <- function(path) {
    if (fs::is_absolute_path(path)) path else file.path(root, path)
  }
  current_absolute <- absolute_path(current)
  new_absolute <- absolute_path(new)

  if (!file.exists(current_absolute)) {
    stop(paste0("File does not exist: ", current))
  }
  if (file.exists(new_absolute)) {
    stop(paste0("New file path already exists: ", new))
  }

  svnCommand(
    .command = "mv",
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
