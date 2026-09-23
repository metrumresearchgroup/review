#' @keywords internal
revision <- function(file=logRoot()){
  info <- tryCatch(svnXML("info", file), error = identity)
  if (inherits(info, "error")) {
    warning(sprintf(
      "running 'svn info' on %s failed: %s",
      file,
      info[["stderr"]]
    ))
    return(NA_real_)
  }

  rev <- info[["entry"]][["commit"]][[".attrs"]][["revision"]]
  if (is.null(rev)) {
    return(NA_real_)
  }
  return(as.numeric(rev))
}

