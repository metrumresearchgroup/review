#' @keywords internal
formatDirSummary <- function(.data) {
  
  dir_summary_data <- .data
  returnList <- list()
  
  dir_summary_data <-
    dir_summary_data %>%
    dplyr::arrange(Author) %>%
    dplyr::filter(Status != "Not in SVN") %>%
    dplyr::mutate(Status = factor(
      Status,
      levels = c("In QC log, needs QC", "QC up to date", "Not in QC log")
    ))
  returnList[["dirSummary"]] <- dir_summary_data
  
  qc_status <-
    dir_summary_data %>%
    dplyr::arrange(Author, File) %>% 
    dplyr::add_count(Author, Status) %>% 
    dplyr::mutate(Author = paste0(Author, " (N=", n, ")")) %>% 
    dplyr::select(Author, File, Status)
  returnList[["qcStatus"]] <- qc_status
  
  return(returnList)
  
}

