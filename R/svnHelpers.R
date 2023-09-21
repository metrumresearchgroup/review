#' @keywords internal
svnCommand <- function(.command,.file = NULL, .quiet = TRUE) {
  
  command_run <- paste("svn",
                       .command,
                       ifelse(.quiet, "2>/dev/null", ""),
                       "--xml",
                       .file,
                       sep = " ")
  
  temp_loc <-
    tryCatch(
      system(command_run, intern = TRUE),
      error = identity
    ) %>% 
    suppressWarnings() %>% 
    suppressMessages()
  
  if (!is.null(attr(temp_loc, "errmsg"))) {
    stop(attr(temp_loc, "errmsg"))
  }
  
  parsed_results <- XML::xmlParse(temp_loc)
  list_results <- XML::xmlToList(parsed_results)
  
  return(list_results)
}
