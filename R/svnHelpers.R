#' @keywords internal
svnCommand <- function(.file, .command, .flags = NULL) {
  
  temp_loc <- file.path(tempdir(), "repository_log.xml")
  flags <- paste0(c(rep("-", length(.flags))), .flags, collapse = " ")
  
  if(!is.null(.flags)) {
    flags <- paste0(flags, " ")
  }
  
  command <- paste0("svn ", .command, " ", flags, "--xml ", .file, " > ", temp_loc)
  system(command)
  
  parsedLog <- XML::xmlParse(temp_loc)
  listLog <- XML::xmlToList(parsedLog)
  
  return(listLog)
}