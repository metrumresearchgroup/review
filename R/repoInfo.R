#' @keywords internal
repoInfo <- function(file=logRoot()) {
  paste(system(paste("svn info --xml",logRoot()),intern=TRUE),collapse="")
}