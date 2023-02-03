#' Visual diff of last QCed version of a file to the local version.
#' 
#' @description 
#' Compares the local version of a script with the most recent QCed version.
#' The output will appear in the viewer and only rows where there have been
#' additions, deletions or modifications in the script will be shown.
#'
#' @param .file file path from working directory
#' @usage 
#' getQcedDiff(.file = "script/data-assembly.Rmd")
#' 
#' @export
getQcedDiff <- function(.file){
  
  .qced_revision <- getQcedRevision(.file)
  
  .qced_temp_file <- tempfile(fileext = glue::glue(".{tools::file_ext(.file)}"))
  
  system(glue::glue("svn export -r {.qced_revision} {.file} {.qced_temp_file}"))
  
  diffobj::diffFile(
    target = .qced_temp_file,
    current = .file, 
    color.mode = "rgb",
    mode = "sidebyside",
    tar.banner = glue::glue("{basename(.file)}: Revision {.qced_revision}"),
    cur.banner = glue::glue("{basename(.file)}: Local")
  )
  
  
  
}