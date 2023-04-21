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
#' @param .file_1 file path of first file from working directory
#' @param .file_2 file path of second file from working directory
#' @param .banner_1 Header for first file in viewer
#' @param .banner_2 Header for second file in viewer
#' @examples 
#' \dontrun{
#' diffFiles(.file_1 = "script/data-assembly-v2.Rmd", 
#'           .file_2 = "script/data-assembly-v1.Rmd")
#' }
#' 
#' @export
diffFiles <- function(.file_1, 
                      .file_2, 
                      .banner_1 = NULL,
                      .banner_2 = NULL) {
  
  if (is.null(.banner_1)) {
    .banner_1 = basename(.file_1)
  }
  
  if (is.null(.banner_2)) {
    .banner_2 = basename(.file_2)
  }
  
  diffobj::diffFile(
    target = .file_1,
    current = .file_2, 
    color.mode = "rgb",
    mode = "sidebyside",
    tar.banner = .banner_1,
    cur.banner = .banner_2
  )
  
}