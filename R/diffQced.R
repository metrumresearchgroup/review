#' Visual diff of last QCed version of a file to the local version.
#' 
#' @description 
#' Compares the local version of a script with the most recent QCed version.
#' The output will appear in the viewer and only rows where there have been
#' additions, deletions or modifications in the script will be shown.
#'
#' @param .file file path from working directory
#' 
#' @examples 
#' \dontrun{
#' diffQced(.file = "script/data-assembly.Rmd")
#' }
#' 
#' @export
diffQced <- function(.file){
  
  diffPreviousRevisions(.file = .file, .previous_revision = getQcedRevision(.file))

}