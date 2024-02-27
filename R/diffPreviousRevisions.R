#' Visual diff of previous revision of a file to the local version.
#' 
#' @description 
#' Compares the current version of a script with a previous revision.
#' The output will appear in the viewer and only rows where there have been
#' additions, deletions or modifications in the script will be shown.
#'
#' @param .file file path from working directory
#' @param .previous_revision revision to compare to current revision
#' @param .current_revision current revision (defaults to local copy)
#' @param .side_by_side Logical. Should diffs be displayed side by side?
#' @param .ignore_white_space Logical. Should white space be ignored?
#' 
#' @examples 
#' with_demoRepo({
#'  diffPreviousRevisions(.file = "script/data-assembly.R", 
#'                        .previous_revision = 2)
#' })
#' 
#' @export
diffPreviousRevisions <- function(.file,
                                  .previous_revision,
                                  .current_revision = NULL,
                                  .side_by_side = TRUE,
                                  .ignore_white_space = FALSE){

  previousCurrent <-
    getPreviousCurrent(
      .file = .file,
      .previous_revision = .previous_revision,
      .current_revision = .current_revision
    )
  
  if (tools::md5sum(previousCurrent$.previous_revision_temp_file) == tools::md5sum(previousCurrent$.current_revision_temp_file)) {
    message("Specified files are identical")
    return(invisible(NULL))
  }
  
  diffFiles(
    .file_1 = previousCurrent$.previous_revision_temp_file, 
    .file_2 = previousCurrent$.current_revision_temp_file, 
    .banner_1 = previousCurrent$.previous_revision_header,
    .banner_2 = previousCurrent$.current_revision_header, 
    .side_by_side = .side_by_side, 
    .ignore_white_space = .ignore_white_space 
  )
  
}
