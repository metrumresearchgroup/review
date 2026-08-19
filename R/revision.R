#' @keywords internal
revision <- function(file=logRoot()){
  info <- svnXML("info", file)
  rev <- info[["entry"]][["commit"]][[".attrs"]][["revision"]]
  if (is.null(rev)) {
    return(NA_real_)
  }
  return(as.numeric(rev))
}

