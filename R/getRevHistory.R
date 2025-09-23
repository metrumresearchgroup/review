#' @noRd
getRevHistory <- function(.file) {
  
  .svn_log <-
    tryCatch(
      svnLog(.file),
      error = identity
    )
  
  if (inherits(.svn_log, "error")) {
    stop(paste0("'", .file, "' is not checked into SVN."))
  }
  
  qclog   <- logRead()
  
  qclog_file <- qclog[qclog$file == .file,]
  .svn_log$datetime <- format(.svn_log$datetime, "%Y-%m-%d %H:%M:%S")
  
  if (nrow(qclog_file) == 0) {
    .svn_log$QCed <- "No"
  } else {
    maxQCrev <- max(qclog_file$revf)
    .svn_log$QCed <- ifelse(as.numeric(.svn_log$rev) <= maxQCrev, "Yes", "No")
  }
  
  .svn_log$rev <- as.numeric(.svn_log$rev)
  .svn_log <- .svn_log[order(-.svn_log$rev), ]
  .svn_log
}