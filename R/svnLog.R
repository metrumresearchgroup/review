#' Get the SVN history of a file
#' 
#' @description 
#' Returns the `svn log` in dataframe format.
#' 
#' @param .file File path of the file of interest
#' 
#' @export
svnLog <- function(.file) {
  
  log_df <- svnCommand(.file = .file, .command = "log")
  log_return <- 
    dplyr::bind_rows(log_df) %>% 
    dplyr::rename(rev = .attrs) %>% 
    dplyr::mutate(datetime = as.POSIXct(date, format="%Y-%m-%dT%H:%M:%OS", tz="UTC")) %>% 
    dplyr::select(author, datetime, rev, msg)
  
  return(log_return)
}
