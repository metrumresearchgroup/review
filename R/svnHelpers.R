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

#' Run 'svn ...' and return standard output.
#'
#' @param ... Arguments passed to svn.
#' @noRd
svnOutput <- function(...) {
  processx::run(
    command = "svn",
    args = purrr::flatten_chr(list(...)),
    error_on_status = TRUE
  )[["stdout"]]
}

#' Run 'svn {subcommand} --xml ...', returning the parsed XML as a list.
#'
#' @param subcommand An svn subcommand.
#' @param ... Arguments passed to the subcommand.
#' @noRd
svnXML <- function(subcommand, ...) {
  args <- c(subcommand, "--xml", purrr::flatten_chr(list(...)))
  proc <- processx::run(
    command = "svn",
    args = args,
    error_on_status = TRUE,
  )
  out <- proc[["stdout"]]

  return(XML::xmlToList(XML::xmlParse(out)))
}
