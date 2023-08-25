#' @keywords internal
svnCommand <- function(.file, .command) {
  
  temp_loc <- file.path(tempfile(fileext = ".xml"))
  
  command <- paste0("svn ", .command, " ", "--xml ", .file, " > ", temp_loc)
  system(command)
  
  parsedLog <- XML::xmlParse(temp_loc)
  listLog <- XML::xmlToList(parsedLog)
  
  return(listLog)
}
