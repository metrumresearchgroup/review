#' QC Summary of Files within a Directory
#'
#' This function provides a QC summary of relevant files in a specified directory.
#' It checks if the file has been checked into SVN, its latest modification date, 
#' author, revision and QC status.
#'
#' @param .dir Character string. Path to the directory to perform the QC summary on.
#'
#' @return A list containing:
#'   * `project`: the name of the project repository.
#'   * `data`: a data.frame containing information about the relevant files including:
#'     - `File`: the name of the file.
#'     - `Author`: the last author to modify the file.
#'     - `Latest edit`: the latest date and time of modification.
#'     - `Latest rev`: the last revision number.
#'     - `Status`: indicating whether the file is in SVN, in QC log, and if it needs QC.
#'     - `QCer`: the reviewer name if the file is in QC log.
#'   * `directory`: the provided directory.
#'
#' @seealso 
#' \code{\link[review]{logSummary}}, \code{\link[review]{svnLog}}
#'
#' @examples 
#' \dontrun{
#' # Assuming appropriate setup and the presence of required 
#' # helper functions like logSummary and svnLog.
#' # dirSummary("/path/to/directory")
#' }
#'
#' @export
dirSummary <- function(.dir) {
  
  project_name <- tryCatch(basename(logRoot()), error = identity)
  
  if (inherits(project_name, "error")) {
    stop("No QC log found")
  }
  
  returnList <- list(
    project = project_name
  )
  
  # Gather files to scan ----------------------------------------------------
  all_files <- list.files(.dir, full.names = TRUE, recursive = TRUE)
  
  relevant_file_types <- c("R", "Rmd", "yaml", "yml", "ctl", "cpp", "cp", "mod", "stan")
  
  extensions <- tools::file_ext(all_files)
  
  relevant_files <- all_files[extensions %in% relevant_file_types] %>% pathFromLogRoot()
  
  relevant_files_df <- dplyr::tibble(
    file = relevant_files,
    lastauthor = NA_character_,
    lastedit = NA_real_,
    lastrev = NA_real_,
    insvn = NA_character_,
  )
  
  
  # Determine current log state ---------------------------------------------
  log_summary <- review::logSummary()
  
  # Build data --------------------------------------------------------------
  relevant_files_df <- relevant_files_df %>% dplyr::left_join(log_summary, by = "file")
  
  n_iter <- nrow(relevant_files_df)
  cli::cli_progress_bar("Checking files", total = n_iter)
  
  for (i in 1:n_iter) {
    
    cli::cli_progress_update()
    
    log.i <- tryCatch(
      review::svnLog(relevant_files_df$file[i]),
      error = identity
    )
    
    if (inherits(log.i, "error")) {
      
      relevant_files_df$insvn[i] <- "No"
      
      next
    }
    
    log.i <- log.i %>% dplyr::filter(datetime == max(datetime))
    
    relevant_files_df$lastauthor[i] <- log.i$author
    relevant_files_df$lastedit[i] <- log.i$datetime
    relevant_files_df$lastrev[i] <- log.i$rev
    relevant_files_df$insvn[i] <- "Yes"
    
    rm(log.i)
  }
  
  cli::cli_progress_done()
  
  # Final cleanup -----------------------------------------------------------
  relevant_files_df <-
    relevant_files_df %>%
    dplyr::transmute(
      File = file,
      Author = lastauthor,
      `Latest edit` = as.POSIXct(lastedit, origin = "1970-01-01", tz = "UTC"),
      `Latest rev` = lastrev,
      Status = dplyr::case_when(
        insvn == "No" ~ "Not in SVN",
        is.na(reviewer) ~ "Not in QC log",
        headf > revf | heado > revo ~ "In QC log, needs QC",
        TRUE ~ "QC up to date"
      ),
      QCer = reviewer
    )
  
  returnList[["data"]] <- relevant_files_df
  returnList[["directory"]] <- .dir
  
  return(returnList)
}
