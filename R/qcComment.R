#' Search for QC Comments in Script
#' 
#' @description Helper to find lines that use the qcComment() function.
#'
#' @param comment Character string containing the QC note
#' @return Invisibly returns the formatted QC comment string
#' @export
#'
#' @examples
#' # In code you would write:
#' # qcComment("Check outlier threshold")
#' # review::qcComment("Check outlier threshold") 
qcComment <- function(comment) {
  if (!is.character(comment) | length(comment) != 1) {
    stop("comment must be a single character string")
  }
  invisible(comment)
}
