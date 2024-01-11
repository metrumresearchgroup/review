#' Get local and svn username
#' 
#' @description 
#' Returns the user's SVN username, along with the repository root, hostname
#' and local username.
#' 
#' @param .host_name Host name where repository is hosted
#' 
#' @keywords internal
svnUser <- function(.host_name = "mc1.metrumrg.com"){
  
  info_list <- tryCatch(
    svnCommand(.command = "info"),
    error = identity
  )
  
  if (inherits(info_list, "error")) {
    stop("svn info failed")
  }
  
  sys_user <- Sys.info()[["user"]]
  
  svn_root <- info_list$entry$repository$root
  
  svn_ssh_user <- 
    unlist(
      strsplit(
        svn_root,
        split = paste0("@", .host_name), 
        fixed = TRUE
      )
    )[1]
  
  svn_user <- unlist(strsplit(svn_ssh_user, "svn+ssh://", fixed = TRUE))[2]
  
  if (is.na(svn_user)) {
    stop("svn user could not be detected from repository root '", svn_root, "'")
  }
  
  list(
    host = .host_name,
    root = svn_root,
    sys = sys_user,
    svn = svn_user
  )
  
}