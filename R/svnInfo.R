#' Get the SVN info of a file
#' 
#' @description 
#' Returns the `svn info` in dataframe format.
#' 
#' @param .file File path of the file of interest
#' 
#' @export
svnInfo <- function(.file){
  
  info_list <- tryCatch(
    svnCommand(.file = .file, .command = "info"),
    error = identity
  )
  
  if (inherits(info_list, "error")) {
    stop("svn info failed")
  }
  
  info_return <- 
    dplyr::bind_rows(info_list$entry$commit) %>% 
    dplyr::rename(rev = .attrs) %>% 
    dplyr::mutate(datetime = as.POSIXct(date, format="%Y-%m-%dT%H:%M:%OS", tz="UTC")) %>% 
    dplyr::select(author, datetime, rev)
  
  info_return
}
