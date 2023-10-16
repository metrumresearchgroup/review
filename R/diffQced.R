#' Visual diff of last QCed version of a file to the local version.
#' 
#' @description 
#' Compares the local version of a script with the most recent QCed version.
#' The output will appear in the viewer and only rows where there have been
#' additions, deletions or modifications in the script will be shown.
#'
#' @param .file file path from working directory
#' @param .side_by_side Logical. Should diffs be displayed side by side?
#' @param .ignore_white_space Logical. Should white space be ignored?
#' 
#' @examples 
#' \dontrun{
#' diffQced(.file = "script/data-assembly.Rmd")
#' }
#' 
#' @export
diffQced <- function(.file, .side_by_side = TRUE, .ignore_white_space = FALSE){
  
  up_to_date <-
    tryCatch(
      svnCommand("status", .file, "-u"),
      error = identity
    )
  
  if (!inherits(up_to_date, "error")) {
    if (!is.null(up_to_date$target$entry)) {
      stop("Please svn up '", .file, "' before running comparison")
    }
  }
  
  if (is.logical(getQcedRevision("file.txt") %>% suppressWarnings())) {
    stop("File has no previous QC")
  }
  
  file_info <- svnLog(.file) %>% dplyr::slice(1)
  
  cli::cli_h1("diffQced info")
  cli::cli_alert_info(paste0("Last author: ", file_info$author))
  cli::cli_alert_info(paste0("Previous edit revision: ", file_info$rev))
  
  diffPreviousRevisions(
    .file = .file, 
    .previous_revision = getQcedRevision(.file),
    .side_by_side = .side_by_side, 
    .ignore_white_space = .ignore_white_space
  )
  
}