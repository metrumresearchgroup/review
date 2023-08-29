#' @keywords internal
svnCommand <- function(.command, .file = NULL) {
  
  command <- paste("svn", .command, "--xml", .file, sep = " ")
  temp_loc <- system(command, intern = TRUE)
  parsed_results <- XML::xmlParse(temp_loc)
  list_results <- XML::xmlToList(parsed_results)
  
  return(list_results)
}
