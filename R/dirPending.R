#' Check if Files in Directory are in QC Log
#'
#' @description 
#' Helper function to ensure all files needing QC are added to the QC log. 
#' Provide a directory and it will be recursively searched for all files that
#' exist within it. Each file will be checked if it exists in the QC log and if
#' it is returned by `logPending()`.
#' 
#' @param .dir Path to directory containing files to search
#'
#' @examples 
#'\dontrun{
#' dirPending(here::here("script"))
#'}
#' @export
dirPending <- function(.dir) {
  
  all_files <- pathFromLogRoot(list.files(.dir, full.names = TRUE, recursive = TRUE))
  extensions <- tools::file_ext(all_files)
  all_files <- all_files[extensions %in% c("R", "Rmd", "yaml", "yml", "ctl", "cpp", "cp", "mod", "stan")]
  
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
      `QCed` = dplyr::if_else(file %in% qc_log & !file %in% pending_scripts, "Y", "N")
    )
  
  return(script_qc)
  
}