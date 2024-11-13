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
  
  svn_log_df <-
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
  
  # Find the common prefix among paths
  common_prefix <- fs::path_common(svn_log_df$file)
  
  # Remove the common prefix from paths
  normalized_paths <- fs::path_rel(svn_log_df$file, start = common_prefix)
  
  # Convert to absolute paths relative to .logRoot
  abs_paths <- fs::path_abs(normalized_paths, start = logRoot())
  
  # Compute relative paths with respect to .logRoot
  svn_log_df$file <- as.character(fs::path_rel(abs_paths, start = logRoot()))
  
  svn_log_df
}