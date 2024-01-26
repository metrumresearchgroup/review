#' Get svn project info
#' 
#' @description 
#' Returns info about the svn project
#' 
#' @param .host_name Host name where repository is hosted
#' 
#' @keywords internal
svnProjInfo <- function(.host_name = "mc1.metrumrg.com"){
  
  info_list <- tryCatch(
    svnCommand(.command = "info"),
    error = identity
  )
  
  if (inherits(info_list, "error")) {
    stop("svn info failed")
  }
  
  svn_url <- info_list$entry$url
  svn_rev <- as.numeric(info_list$entry$commit$.attrs)
  
  svn_ssh_user <- 
    unlist(
      strsplit(
        svn_url,
        split = paste0("@", .host_name), 
        fixed = TRUE
      )
    )[1]
  
  svn_user <- unlist(strsplit(svn_ssh_user, "svn+ssh://", fixed = TRUE))[2]
  
  if (is.na(svn_user)) {
    stop("svn user could not be detected from repository URL '", svn_url, "'")
  }
  
  list(
    this_svn_user = svn_user,
    host = .host_name,
    url = svn_url,
    rev = svn_rev,
    rev_author = info_list$entry$commit$author,
    rev_datetime = info_list$entry$commit$date
  )
  
}
