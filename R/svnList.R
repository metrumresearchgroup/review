#' Get the SVN list of a repo
#' 
#' @description 
#' Returns the `svn list` in dataframe format.
#' 
#' @noRd
svnList <- function(){
  
  svn_list <- tryCatch(
    svnCommand("list", .flags = "--depth infinity"),
    error = identity
  )
  
  if (inherits(svn_list, "error")) {
    stop("svn list failed")
  }
  
  list_return <- 
    dplyr::bind_rows(svn_list$list) %>% 
    tidyr::unnest(commit) %>% 
    dplyr::select(name, commit) %>% 
    dplyr::group_by(name) %>% 
    dplyr::summarise(value = paste0(commit, collapse = "__reviewsep__")) %>% 
    dplyr::ungroup() %>% 
    tidyr::separate(value, c("author", "datetime", "rev"), sep = "__reviewsep__") %>% 
    dplyr::mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%dT%H:%M:%OS", tz="UTC")) %>% 
    dplyr::select(file = name, author, datetime, rev)
  
  list_return
}
