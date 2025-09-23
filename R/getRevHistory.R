#' @noRd
getRevHistory <- function(.file) {
  .svn_log <- svnLog(.file)

  qclog <- logRead()

  qclog_file <- qclog[qclog$file == .file, ]
  qclog_file <- qclog_file[qclog_file$revf != 0, ]
  .svn_log$datetime <- format(.svn_log$datetime, "%Y-%m-%d %H:%M:%S")

  .svn_log$QCed <- if (nrow(qclog_file) == 0) {
    "No"
  } else {
    ifelse(as.numeric(.svn_log$rev) <= max(qclog_file$revf), "Yes", "No")
  }

  .svn_log$rev <- as.numeric(.svn_log$rev)
  .svn_log <- .svn_log[order(-.svn_log$rev), ]
  .svn_log
}
