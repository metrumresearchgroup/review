#' Rename file in SVN and QClog
#' 
#' @description 
#' This function facilitates renaming a file in SVN. When run
#' the file will be renamed and a targeted commit to SVN will
#' be made to check in the new name. Additionally, if the file
#' exists in the QC log, all entries will be renamed and the updated
#' QC log will also be checked in to SVN.
#' 
#' @param .filepath current file path of file to be renamed
#' @param .new_filepath new file path for file
#' 
#' @export
fileRename <- function(.filepath, .new_filepath) {
  
  # Check current file path exists
  if(!file.exists(.filepath)) {
    stop(paste0(.filepath, " does not exist"))
  }
  
  # Check if new file path already exists
  if(file.exists(.new_filepath)) {
    stop(paste0(.new_filepath, " already exists, can't overwrite"))
  }
  
  system(
    glue::glue("svn mv {.filepath} {.new_filepath}")
  )
  
  qclog <- logRead()
  qclog[qclog$file == .filepath,][["file"]] <- .new_filepath
  qclog[qclog$origin == .filepath,][["origin"]] <- .new_filepath
  
  logWrite(qclog, file = "QClog.csv")
  
  system(
    glue::glue(
      "svn commit -m 'rename {.filepath}' {.filepath} {.new_filepath} QClog.csv"
    )
  )
}
