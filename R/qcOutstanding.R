#' Count Outstanding QC Items in Scripts
#' 
#' @description Returns details of QC comments in one or more script files.
#'
#' @param files Character vector of file paths
#' @return A data frame with columns:
#'   \item{script_name}{Name of the script}
#'   \item{qc_comment}{The QC comment text}
#'   \item{line_number}{Line number where comment appears}
#' @export
#'
#' @examples
#' # Create a temporary file with some QC comments
#' tf <- tempfile(fileext = ".R")
#' writeLines(c(
#'   "x <- 1",
#'   'qcComment("check this")',
#'   'review::qcComment("another check")'
#' ), tf)
#' 
#' # Find QC comments
#' qcOutstanding(tf)
#' 
#' # Clean up
#' unlink(tf)
qcOutstanding <- function(files) {
  if (!is.character(files)) {
    stop("files must be a character vector of file paths")
  }
  if (!all(file.exists(files))) {
    stop("Some specified files do not exist")
  }
  
  file_results <- list()
  
  for (i in seq_along(files)) {
    lines <- readLines(files[i])
    
    # Find lines containing qcComment calls
    # Pattern explanation:
    # (review::)?     Optionally match "review::" at the start
    # qcComment\\(    Match "qcComment("
    qc_lines <- grep("(review::)?qcComment\\(", lines)
    
    if (length(qc_lines) > 0) {
      # Extract the comment text from within the qcComment call
      # Pattern explanation:
      # .*                     Match any characters from start of line
      # (?:review::)?          Optionally match "review::" (non-capturing group)
      # qcComment\\("         Match 'qcComment("'
      # (.+)                   Capture one or more characters (the comment text)
      # "\\).*                Match '")' and any remaining characters
      comments <- sub('.*(?:review::)?qcComment\\("(.+)"\\).*', "\\1", lines[qc_lines], perl = TRUE)
      
      file_results[[i]] <- data.frame(
        script_name = basename(files[i]),
        qc_comment = comments,
        line_number = qc_lines,
        stringsAsFactors = FALSE
      )
    }
  }
  
  do.call(rbind, c(file_results, make.row.names = FALSE))
}
