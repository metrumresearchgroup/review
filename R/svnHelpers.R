#' @keywords internal
svnCommand <- function(.command, .file = NULL, .flags = NULL, .quiet = TRUE) {
  
  command_run <- paste("svn",
                       .command,
                       .flags,
                       "--xml",
                       .file,
                       ifelse(.quiet, "2>/dev/null", ""),
                       sep = " ")
  
  temp_loc <- system(command_run, intern = TRUE) %>% suppressWarnings()
  
  if (!is.null(attr(temp_loc, "status"))) {
    stop("svn command failed")
  }
  
  parsed_results <- XML::xmlParse(temp_loc)
  list_results <- XML::xmlToList(parsed_results)
  
  list_results
}
