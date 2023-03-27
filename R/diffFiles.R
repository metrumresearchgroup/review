#' Visual diff between two different files
#' 
#' @description 
#' Compares the current version of two scripts against one another.
#' The output will appear in the viewer and only rows where there have been
#' additions, deletions or modifications in the script will be shown.
#' 
#' This should be used when a new script has been created based off
#' an existing script.
#'
#' @param .file_current file path of new file from working directory
#' @param .file_previous file path of previous file from working directory
#' @param .current_revision_header Header for current file in viewer (default: "Current file")
#' @param .previous_revision_header Header for current file in viewer (default: "Previous file")
#' @examples 
#' \dontrun{
#' diffFiles(.file_current = "script/data-assembly-v2.Rmd", 
#'           .file_previous = "script/data-assembly-v1.Rmd")
#' }
#' 
#' @export
diffFiles <- function(.file_current, .file_previous, 
                      .current_revision_header = "Current file",
                      .previous_revision_header = "Previous file") {
  
  diffobj::diffFile(
    target = .file_previous,
    current = .file_current, 
    color.mode = "rgb",
    mode = "sidebyside",
    tar.banner = .previous_revision_header,
    cur.banner = .current_revision_header
  )
  
}