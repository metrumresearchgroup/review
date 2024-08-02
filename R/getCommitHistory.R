#' Get Full Commit History
#'
#' @description
#' Returns a data.frame containing every commit made to the repo. 
#' The information recorded includes the author, revision, date and
#' commit message.
#'
#' @export
getCommitHistory <- function() {
  
  svn_all <- tryCatch(
    svnCommand("log", .flags = "-v"),
    error = identity
  )
  
  if (inherits(svn_all, "error")) {
    stop("svn log failed")
  }
  
  dplyr::bind_rows(svn_all) %>% 
    tidyr::unnest(paths) %>% 
    dplyr::filter(names(paths) == "text") %>% 
    tidyr::unnest(paths) %>% 
    dplyr::rename(
      rev = .attrs,
      file = paths
    ) %>% 
    dplyr::mutate(
      date = as.Date(substr(date, 1, 10)),
      file = sub('.', '', file))
  
}