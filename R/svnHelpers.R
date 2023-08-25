#' @keywords internal
svnCommand <- function(.command, .file = NULL) {
  
  command <- paste0("svn ", .command, " ", "--xml ", .file)
  temp_loc <- system(command, intern = TRUE)
  parsedLog <- XML::xmlParse(temp_loc)
  listLog <- XML::xmlToList(parsedLog)
  
  return(listLog)
}
