#' Delete file in SVN and QClog
#' 
#' @description 
#' This function facilitates deleting a file in SVN. When run
#' the file will be deleted and removed from the QC log. A
#' commit will be made with a message describing the updates.
#' 
#' @param .filepath current file path of file to be deleted
#' 
#' @export
fileDelete <- function(.filepath) {
  
  # Check current file path exists
  if(!file.exists(.filepath)) {
    stop(paste0(.filepath, " does not exist"))
  }
  
  system(glue::glue("svn delete {.filepath} -q -q"))
  cli::cli_alert_info(glue::glue("Deleted '{.filepath}'"))
  
  qclog <- logRead()
  qclog <- qclog[qclog$file != .filepath, ]
  
  logWrite(qclog, file = "QClog.csv")
}
