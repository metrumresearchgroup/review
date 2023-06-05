#' Check if Files in Directory are in QC Log
#'
#' @description 
#' Helper function to ensure all files needing QC are added to the QC log. 
#' Provide a directory and it will be recursively searched for all files that
#' exist within it. Each file will be checked if it exists in the QC log and if
#' it is returned by `logPending()`.
#' 
#' @param .dir_path Path to directory containing files to search
#' @param .only_not_in_QC_log Logical. Set to TRUE to have only files not in QC log returned
#'
#' @export
checkDirQC <- function(.dir_path, .only_not_in_QC_log = FALSE) {
  
  all_files <- pathFromLogRoot(list.files(.dir_path, full.names = TRUE, recursive = TRUE))
  
  qc_log <- 
    readr::read_csv(file.path(logRoot(), "QClog.csv")) %>% 
    dplyr::distinct(file) %>% 
    dplyr::pull(file) %>% 
    suppressMessages()
  
  pending_scripts <-
    review::logPending() %>% 
    dplyr::distinct(file) %>% 
    dplyr::pull()
  
  script_qc <-
    dplyr::tibble(
      file = all_files,
      `QCLog` = dplyr::if_else(file %in% qc_log, "Y", "N"),
      `QCPending` = dplyr::if_else(file %in% pending_scripts, "Y", "N")
    )
  
  if(.only_not_in_QC_log) {
    script_qc <- 
      script_qc %>% 
      dplyr::filter(QCLog == "N")
  }
  
  # if(.show_summary) {
  #   
  #   y_y <- script_qc %>% dplyr::filter(QCLog == "Y" & QCPending == "N")
  #   y_pending <- script_qc %>% dplyr::filter(QCPending == "Y")
  #   n_log <- script_qc %>% dplyr::filter(QCLog == "N")
  #   
  #   print(
  #     cli::boxx(
  #       header = "QC Log Check Summary",
  #       label = c(
  #         paste0("Number of files in QC log (non-pending): ", nrow(y_y)),
  #         paste0("Number of files pending in QC log: ", nrow(y_pending)),
  #         paste0("Number of files not in QC log: ", nrow(n_log))
  #       ))
  #   )
  #  
  #}
  
  return(script_qc)
  
}