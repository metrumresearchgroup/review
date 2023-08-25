#' Get the SVN history of a file
#' 
#' @description 
#' Returns the raw output of `svn log` and `svn info` for the specified
#' file. `svn log` provides information regarding each revision of the 
#' file and `svn info` provides an overview of the latest update.
#' 
#' @param .file Relative file path of the file of interest
#' 
#' @export
svnMeta <- function(.file) {
  
  return_list <- list()
  
  return_list$log <- svnCommand(.file = .file, .command = "log")
  return_list$info <- svnCommand(.file = .file, .command = "info")
  
  return(return_list)
}
