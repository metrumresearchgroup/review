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
#' @param .file_new file path of new file from working directory
#' @param .file_previous file path of previous file from working directory
#' 
#' @examples 
#' \dontrun{
#' diffNewFile(.file_new = "script/data-assembly-v2.Rmd", 
#'             .file_previous = "script/data-assembly-v1.Rmd")
#' }
#' 
#' @export
diffNewFile <- function(.file_new, .file_previous) {
  
  diffobj::diffFile(
    target = .file_previous,
    current = .file_new, 
    color.mode = "rgb",
    mode = "sidebyside"
  )
  
}