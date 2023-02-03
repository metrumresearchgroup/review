#' Visual diff of previous revision of a file to the local version.
#' 
#' @description 
#' Compares the local version of a script with a previous revision.
#' The output will appear in the viewer and only rows where there have been
#' additions, deletions or modifications in the script will be shown.
#'
#' @param .file file path from working directory
#' @param .previous_revision revision to compare to local file
#' 
#' @examples 
#' \dontrun{
#' getPreviousRevisionDiff(.file = "script/data-assembly.Rmd", .previous_revision = 5)
#' }
#' 
#' @export
getPreviousRevisionDiff <- function(.file, .previous_revision){
  
  .previous_revision_temp_file <- tempfile(fileext = glue::glue(".{tools::file_ext(.file)}"))
  
  system(glue::glue("svn export -r {.previous_revision} {.file} {.previous_revision_temp_file}"))
  
  diffobj::diffFile(
    target = .previous_revision_temp_file,
    current = .file, 
    color.mode = "rgb",
    mode = "sidebyside",
    tar.banner = glue::glue("{basename(.file)}: Revision {.previous_revision}"),
    cur.banner = glue::glue("{basename(.file)}: Local")
  )
  
  
  
}