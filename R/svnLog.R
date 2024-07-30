#' Get the SVN history of a file
#' 
#' @description 
#' Returns the `svn log` in data.frame format. If no file is 
#' provided the full history of commits in the repo is returned. 
#' 
#' @param .file File path of the file of interest
#' 
#' @examples 
#' with_demoRepo({
#'   svnLog("script/data-assembly.R")
#' })
#' 
#' @export
svnLog <- function(.file = NULL) {
  
  log_df <- getCommitHistory()
  
  if (!is.null(.file)) {
    log_df <- log_df %>% dplyr::filter(file == .file)
  }
  
  log_return <- 
    dplyr::bind_rows(log_df) %>% 
    dplyr::select(author, date, rev, msg)
  
  log_return
}
