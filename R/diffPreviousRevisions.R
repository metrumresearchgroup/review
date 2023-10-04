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
#' \dontrun{
#' diffPreviousRevisions(.file = "script/data-assembly.Rmd", .previous_revision = 5)
#' }
#' 
#' @export
diffPreviousRevisions <- function(.file,
                                  .previous_revision,
                                  .current_revision = NULL,
                                  .side_by_side = TRUE,
                                  .ignore_white_space = FALSE){
  
  .previous_revision_temp_dir <- tempdir()
  
  .previous_revision_temp_file <- 
    svnExport(
      .file = .file, 
      .revision = .previous_revision, 
      .output_dir = .previous_revision_temp_dir, 
      .return_file = TRUE,
      .quiet = TRUE
    )
  
  .previous_revision_header <- glue::glue("{basename(.file)}: Revision {.previous_revision}")
  
  if (is.null(.current_revision)) {
    
    .current_revision_temp_file <- tempfile(fileext = glue::glue(".{tools::file_ext(.file)}"))
    
    system(glue::glue("cp {.file} {.current_revision_temp_file}"))
    
    .current_revision_header <- glue::glue("{basename(.file)}: Local")
    
  } else {
    
    .current_revision_temp_dir <- tempdir()
    
    .current_revision_temp_file <- 
      svnExport(
        .file = .file, 
        .revision = .current_revision, 
        .output_dir = .current_revision_temp_dir, 
        .return_file = TRUE,
        .quiet = TRUE
      )
    
    .current_revision_header <- glue::glue("{basename(.file)}: Revision {.current_revision}")
    
  }
  
  if (tools::md5sum(.previous_revision_temp_file) == tools::md5sum(.current_revision_temp_file)) {
    message("Specified files are identical")
    return(invisible(NULL))
  }
  
  diffFiles(
    .file_1 = .previous_revision_temp_file, 
    .file_2 = .current_revision_temp_file, 
    .banner_1 = .previous_revision_header,
    .banner_2 = .current_revision_header, 
    .side_by_side = .side_by_side, 
    .ignore_white_space = .ignore_white_space 
  )
  
}
