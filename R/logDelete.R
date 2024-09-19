#' Delete file from QClog
#' 
#' @description 
#' This function facilitates deleting a file from the QClog.csv file. 
#' When run, all rows associated with the file will be deleted and 
#' removed from the QC log.
#' 
#' @param .filepath current file path of file to be removed
#' 
#' @export
logDelete <- function(.filepath) {
  
  qclog <- logRead()
  qclog2 <- qclog[qclog$file != .filepath, ]
  
  # Check current file path exists
  if(nrow(qclog) == nrow(qclog2)) {
    stop(paste0(.filepath, " does not exist in QClog.csv"))
  }
  
  cli::cli_alert_info(glue::glue("Deleted '{.filepath}' from QClog.csv"))
  
  logWrite(qclog2, file = "QClog.csv")
}
