#' Summarize revision and QC history for a file
#'
#' @description
#' Generates a comprehensive summary of the revision and quality control (QC) history
#' for a file. The function reports:
#' * QC status (up to date, needs QC, or not assigned)
#' * Presence in the QC log
#' * Historical information about previous authors
#' * Historical information about previous QC reviewers
#'
#' @param .file File path.
#'
#' @details
#' The function prints a formatted summary to the console and invisibly returns
#' the data for programmatic use. Non-existent files generate warnings and are
#' skipped. Files not found in the repository history are also skipped.
#'
#' QC status can be:
#' * "QC up to date" - Latest revision has been QC'd
#' * "Needs QC" - New changes need review
#' * "Not assigned" - File not in QC system
#'
#' @examples
#' \dontrun{
#' # Analyze a single script file
#' fileSummary("script/data-assembly/study-101.R")
#'
#' # Process all files in a directory
#' purrr::walk(list.files("script/data-assembly"), ~ fileSummary(.x))
#' }
#'
#' @export
fileSummary <- function(.file) {
  if (length(.file) != 1) {
    stop("'.file' must be a single file path")
  }
  
  if (!file.exists(.file)) {
    warning(paste0(.file, " does not exist"))
    return(invisible(NULL))
  }
  
  # Initialize output list
  out <- list()
  
  # Get repo history and QC log
  file_history <- repoHistory()
  qc_log <- logRead()
  
  # Get repo commit history for the file
  script_history <- file_history[file_history$file == .file, ]
  
  if (nrow(script_history) == 0) {
    warning(paste0(.file, " has no svn history"))
    return(invisible(NULL))
  }
  
  # Get last revision of the file
  last_rev <- as.numeric(script_history$rev[1])
  
  # Filter QC log to file of interest
  qc_file <- qc_log[qc_log$file == .file, ]
  
  # Check if file is in QC log
  if (nrow(qc_file) > 0) {
    out$qclog <- cli::col_green("Yes")
  } else {
    out$qclog <- cli::col_red("No")
    out$qcstatus <- cli::col_magenta("Not assigned")
  }
  
  # Process QC history
  qc_file_filtered <- qc_file %>% dplyr::filter(revf != 0)
  
  if (nrow(qc_file_filtered) > 0) {
    
    qc_rev <- 
      qc_file_filtered %>%
      dplyr::arrange(-as.numeric(revf)) %>%
      dplyr::slice(1) %>%
      dplyr::pull(revf) %>%
      as.numeric()
    
    out$prevQC <- 
      qc_file_filtered %>%
      dplyr::arrange(-as.numeric(revf)) %>%
      dplyr::group_by(reviewer) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(VALUE = paste0(
        reviewer, ", ",
        as.numeric(difftime(Sys.Date(), as.Date(time), units = "days")),
        " days ago"
      )) %>%
      dplyr::pull()
    
    out$qcstatus <- 
      ifelse(
        last_rev > qc_rev,
        cli::col_red("Needs QC"), 
        cli::col_green("QC up to date")
      )
    
  } else {
    out$qcstatus <- cli::col_red("Needs QC")
    out$prevQC <- "No previous QC"
  }
  
  # Process author history
  out$authors <- 
    script_history %>%
    dplyr::group_by(author) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-as.numeric(rev)) %>%
    dplyr::transmute(VALUE = paste0(
      author, ", ",
      as.numeric(difftime(Sys.Date(), date, units = "days")),
      " days ago"
    )) %>%
    dplyr::pull()
  
  # Print summary
  cli::cli_h2(glue::glue(.file, " summary"))
  cli::cli_inform(glue::glue("In QC log: ", out$qclog))
  cli::cli_inform(glue::glue("QC status: ", out$qcstatus))
  
  cli::cli_verbatim("") # line break
  cli::cli_inform("Previous author(s): ")
  names(out$authors) <- rep(">", length(out$authors))
  cli::cli_bullets(out$authors)
  
  cli::cli_verbatim("") # line break
  cli::cli_inform("Previous QCer(s): ")
  names(out$prevQC) <- rep(">", length(out$prevQC))
  cli::cli_bullets(out$prevQC)
  
  return(invisible(out))
}
