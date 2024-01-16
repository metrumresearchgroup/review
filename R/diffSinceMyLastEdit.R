#' Visual diff of changes to a file since user's last modifications.  
#' 
#' @description 
#' Compares the local version of a script with the most recent QCed version.
#' The output will appear in the viewer and only rows where there have been
#' additions, deletions or modifications in the script will be shown.
#'
#' @param .file file path from working directory
#' @param .host_name Host name where repository is hosted
#' @param .side_by_side Logical. Should diffs be displayed side by side?
#' @param .ignore_white_space Logical. Should white space be ignored?
#' 
#' @examples 
#' \dontrun{
#' diffSinceMyLastEdit(.file = "script/data-assembly.Rmd")
#' }
#' 
#' @export
diffSinceMyLastEdit <- function(.file, 
                           .host_name = "mc1.metrumrg.com", 
                           .side_by_side = TRUE, 
                           .ignore_white_space = FALSE) {
  
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
  
  lastEditRev <- as.numeric(authorRevs$rev)
  
  cli::cli_h2(glue::glue("Diff since {userSVN} last edit on: ", .file))
  cli::cli_inform(glue::glue("{userSVN}'s last modified revision: ", lastEditRev))
  
  diffPreviousRevisions(
    .file = .file, 
    .previous_revision = lastEditRev,
    .side_by_side = .side_by_side, 
    .ignore_white_space = .ignore_white_space
  )
  
}