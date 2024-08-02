#' Get the SVN history of a file
#' 
#' @description 
#' Returns the `svn log` in dataframe format.
#' 
#' @param .file File path of the file of interest
#' 
#' @examples 
#' with_demoRepo({
#'   svnLog("script/data-assembly.R")
#' })
#' 
#' @export
svnLog <- function(.file) {
  
  log_df <- tryCatch(
    svnCommand(.file = .file, .command = "log"),
    error = identity
  )
  
  if (inherits(log_df, "error")) {
    stop("svn log failed")
  }
  
  log_return <- 
    dplyr::bind_rows(log_df) %>% 
    dplyr::rename(rev = .attrs) %>% 
    dplyr::mutate(datetime = as.POSIXct(date, format="%Y-%m-%dT%H:%M:%OS", tz="UTC")) %>% 
    dplyr::select(author, datetime, rev, msg)
  
  log_return
}
