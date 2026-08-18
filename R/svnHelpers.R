#' Run 'svn ...' for side-effects.
#'
#' Standard output is discarded, and standard error is displayed only when an
#' error is signaled.
#'
#' @param ... Arguments passed to svn.
#' @noRd
svnRun <- function(...) {
  args <- purrr::flatten_chr(list(...))
  processx::run(
    command = "svn",
    args = args,
    echo = FALSE,
    stdout = NULL,
    error_on_status = TRUE
  )

  return(invisible(NULL))
}

#' @noRd
svnCommand <- function(.command, .file = NULL, .flags = NULL, .quiet = TRUE, .xml = TRUE) {
  
  command_run <- paste("svn",
                       .command,
                       .flags,
                       ifelse(.xml, "--xml", ""),
                       paste0("'", .file, "'"),
                       ifelse(.quiet, "2>/dev/null", ""),
                       sep = " ")
  
  temp_loc <- system(command_run, intern = TRUE) %>% suppressWarnings()
  
  if (!is.null(attr(temp_loc, "status"))) {
    stop("svn command failed")
  }
  
  if (!.xml) {
    return(invisible(NULL))
  }
  
  parsed_results <- XML::xmlParse(temp_loc)
  list_results <- XML::xmlToList(parsed_results)
  
  list_results
}
