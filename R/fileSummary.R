#' Return summary of revision and QC history for files
#' 
#' @description 
#' Returns a summary of the revision and QC history of a file.
#' Users can provide a single file (or multiple) and will be
#' provided with file information such as: QC status, if the 
#' file is in the QC log, past authors and past QCers.
#'
#' @param .file Character vectors of file paths 
#' 
#' @export
fileSummary <- function(.file) {
  
  out_list <- list()
  
  # Only need to run repo history once
  file_history <- repoHistory()
  
  # Read in QC log once
  qc_log <- logRead()
  
  for (file.i in .file) {
    
    if (!file.exists(file.i)) {
      warning(paste0(file.i, " does not exist")) 
      next()
    }
    
    out_list[[file.i]] <- list()
    
    # Get repo commit history
    script_history <- file_history[file_history$file == file.i,]
    
    if (nrow(script_history) == 0) {
      next()
    }
    
    # Get last revision of the file
    last_rev <- as.numeric(script_history$rev[1])
    
    # Filter QC log to file of interest
    qc_file <- qc_log[qc_log$file == file.i,] 
    
    if (nrow(qc_file) > 0) {
      out_list[[file.i]]$qclog <- cli::col_green("Yes")
    } else {
      out_list[[file.i]]$qclog <- cli::col_red("No")
      out_list[[file.i]]$qcstatus <- cli::col_magenta("Not assigned")
    }
    
    # Remove assigning records from QC log
    qc_file_filtered <- qc_file %>% dplyr::filter(revf != 0)
    
    if (nrow(qc_file_filtered) > 0) {
      
      qc_rev <-
        qc_file_filtered %>% 
        dplyr::arrange(-as.numeric(revf)) %>% 
        dplyr::slice(1) %>% 
        dplyr::pull(revf) %>% 
        as.numeric()
      
      prev_qc <-
        qc_file_filtered %>% 
        dplyr::arrange(-as.numeric(revf)) %>% 
        dplyr::group_by(reviewer) %>% 
        dplyr::slice(1) %>% 
        dplyr::ungroup() %>% 
        dplyr::transmute(VALUE = paste0(reviewer, ", ", 
                                        as.numeric(difftime(Sys.Date(), as.Date(time), units = "days")), 
                                        " days ago")) %>% 
        dplyr::pull()
      
      out_list[[file.i]]$prevQC <- prev_qc
      
      if (last_rev > qc_rev) {
        status <- cli::col_red("Needs QC")
      } else {
        status <- cli::col_green("QC up to date")
      }
      
    } else {
      status <- cli::col_red("Needs QC")
      out_list[[file.i]]$prevQC <- "No previous QC"
    }
    
    if (is.null(out_list[[file.i]]$qcstatus)) {
      out_list[[file.i]]$qcstatus <- status
    }
    
    # All authors
    all_authors <- 
      script_history %>% 
      dplyr::group_by(author) %>% 
      dplyr::slice(1) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(-as.numeric(rev)) %>% 
      dplyr::transmute(VALUE = paste0(author, ", ", 
                                      as.numeric(difftime(Sys.Date(), date, units = "days")), 
                                      " days ago")) %>% 
      dplyr::pull()
    
    out_list[[file.i]]$authors <- all_authors
    
    cli::cli_h2(glue::glue(file.i, " summary"))
    cli::cli_inform(glue::glue("In QC log: ", out_list[[file.i]]$qclog))
    cli::cli_inform(glue::glue("QC status: ", out_list[[file.i]]$qcstatus))
    cli::cli_inform("Previous author(s): ")
    
    names(out_list[[file.i]]$authors) <- rep(">", length(out_list[[file.i]]$authors))
    
    cli::cli_bullets(out_list[[file.i]]$authors)
    
    cli::cli_inform("Previous QCer(s): ")
    
    names(out_list[[file.i]]$prevQC) <- rep(">", length(out_list[[file.i]]$prevQC))
    
    cli::cli_bullets(out_list[[file.i]]$prevQC)
    
  }
  
  return(invisible(out_list))
}