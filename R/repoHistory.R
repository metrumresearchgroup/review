#' Get Repository History
#'
#' @description
#' Returns a data.frame containing every commit made to the repo. 
#' The information recorded includes the author, revision, date and
#' commit message.
#'
#' @export
repoHistory <- function() {
  
  svn_log_v <- tryCatch(
    svnCommand("log", .flags = "-v"),
    error = identity
  )
  
  if (inherits(svn_log_v, "error")) {
    stop("svn log failed")
  }
  
  dplyr::bind_rows(svn_log_v) %>% 
    tidyr::unnest(paths) %>% 
    dplyr::filter(names(paths) == "text") %>% 
    tidyr::unnest(paths) %>% 
    dplyr::rename(
      rev = .attrs,
      file = paths
    ) %>% 
    dplyr::mutate(
      date = as.Date(date),
      file = sub('.', '', file),
      rev = as.integer(rev)
    )
  
}