#' Determine most recent QCed revision number for a file
#' 
#' @description 
#' Returns the most recent revision number of when the specified file was
#' accepted in the QClog.
#'
#' @param .file file path from working directory
#' @usage 
#' getQcedRevision(.file = "script/data-assembly.Rmd")
#' 
#' @importFrom dplyr %>%
#' 
#' @export
getQcedRevision <- function(.file){
  
  stopifnot(file.exists(.file))
  
  qc_log <- logRead()
  
  project_file_path <- logTarget(.file)
  project_file_path <- 
    gsub(pattern = paste0(logRoot(), "/"), replacement = "", x = project_file_path, fixed = TRUE)
  
  qced_revision <- 
    qc_log %>% 
    dplyr::filter(
      stringr::str_detect(file, project_file_path)
    ) %>% 
    dplyr::filter(
      revf == max(revf)
    ) %>% 
    dplyr::pull(revf)
  
  return(qced_revision)
  
}