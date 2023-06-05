#' Check if Files in Directory are in QC Log
#'
#' @description 
#' Helper function to ensure all files needing QC are added to the QC log. 
#' Provide a directory and it will be recursively searched for all files that
#' exist within it. Each file will be checked if it exists in the QC log and if
#' it is returned by `logPending()`.
#' 
#' @param .dir_path Path to directory containing files to search
#'
#' @examples 
#'\dontrun{
#' checkDirQC(here::here("script"))
#'}
#' @export
checkDirQC <- function(.dir_path) {
  
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
  
  return(script_qc)
  
}