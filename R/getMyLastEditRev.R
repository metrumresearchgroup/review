#' Find revision of user's last modification 
#' 
#' @description 
#' Compares the local version of a script with the most recent version the user
#' committed. This is a helpful tool when reviewing the changes made after QC
#' comments have been addressed.
#'
#' @param .file file path from working directory
#' @param .host_name Host name where repository is hosted
#' 
#' @examples 
#' \dontrun{
#' getMyLastEditRev(.file = "script/data-assembly.Rmd")
#' }
#' 
#' @keywords internal
getMyLastEditRev <- function(.file, .host_name = "mc1.metrumrg.com") {
  
  output_list <- list()
  
  # Search SVN log 
  logFile <- svnLog(.file)
  userSVN <- svnUser(.host_name = .host_name)[["svn"]]
  
  authorRevs <-
    logFile %>% 
    dplyr::filter(author == userSVN) %>% 
    dplyr::slice(1)
  
  if (nrow(authorRevs) == 0) {
    cli::cli_abort(glue::glue("User has never modified '{.file}'"))
  }
  
  output_list[["file"]] <- .file
  output_list[["usernameSVN"]] <- userSVN
  output_list[["revision"]] <- as.numeric(authorRevs$rev)
  output_list[["datetime"]] <- authorRevs$datetime
  
  return(output_list)
}