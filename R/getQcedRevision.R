#' Determine most recent QCed revision number for a file
#' 
#' @description 
#' Returns the most recent revision number of when the specified file was
#' accepted in the QClog.
#'
#' @param .file file path from working directory
#' @examples  
#' with_demoRepo({
#'   getQcedRevision(.file = "script/data-assembly.R")
#' })
#' 
#' @export
getQcedRevision <- function(.file){
  
  stopifnot(file.exists(.file))
  
  qc_log <- logRead()
  
  if (suppressWarnings(anyNA(as.numeric(qc_log$revf)))) {
    
    stop("Non numeric values found in 'revf' column of QC log")
    
  }
  
  project_file_path <- logTarget(pathFromLogRoot(.file))
  project_file_path <- tools::file_path_as_absolute(project_file_path)
  project_file_path <- 
    gsub(pattern = paste0(logRoot(), "/"), replacement = "", x = project_file_path, fixed = TRUE)
  
  qced_revision <- 
    qc_log %>% 
    dplyr::filter(revf != 0) %>% 
    dplyr::filter(stringr::str_detect(file, project_file_path))
  
  if (nrow(qced_revision) == 0) {
    return(NA_real_)
  }
  
  qced_revision <-
    qced_revision %>% 
    dplyr::filter(revf == max(revf)) %>% 
    dplyr::distinct(revf, .keep_all = TRUE) %>% 
    dplyr::pull(revf)
  
  qced_revision
  
}