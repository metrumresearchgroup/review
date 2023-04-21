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
#' 
#' @examples 
#' \dontrun{
#' diffPreviousRevisions(.file = "script/data-assembly.Rmd", .previous_revision = 5)
#' }
#' 
#' @export
diffPreviousRevisions <- function(.file, .previous_revision, .current_revision = NULL){
  
  .previous_revision_temp_file <- tempfile(fileext = glue::glue(".{tools::file_ext(.file)}"))
  
  system(glue::glue("svn export -r {.previous_revision} {.file} {.previous_revision_temp_file} -q"))
  
  .previous_revision_header <- glue::glue("{basename(.file)}: Revision {.previous_revision}")
  
  .current_revision_temp_file <- tempfile(fileext = glue::glue(".{tools::file_ext(.file)}"))
  
  if (is.null(.current_revision)) {
    
    system(glue::glue("cp {.file} {.current_revision_temp_file}"))
    
    .current_revision_header <- glue::glue("{basename(.file)}: Local")
    
  } else {
    
    system(glue::glue("svn export -r {.current_revision} {.file} {.current_revision_temp_file} -q"))
    
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
    .banner_2 = .current_revision_header 
  )

}
