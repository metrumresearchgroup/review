#' @noRd
findAuthorsQcers <- function(.repoHistory, .qcLog) {
  
  qclog <- 
    .qcLog %>% 
    dplyr::transmute(file, rev = as.integer(revf), reviewer) %>% 
    dplyr::filter(rev != "0") %>% 
    dplyr::distinct()
  
  rH_qclog <-
    .repoHistory %>%
    dplyr::left_join(qclog, by = c("file", "rev")) %>%
    dplyr::arrange(file, -rev) %>% 
    dplyr::group_by(file) %>% 
    tidyr::fill(reviewer, .direction = "down") %>%
    dplyr::ungroup()
  
  out <- list()
  
  out$authors <- rH_qclog %>% dplyr::filter(is.na(reviewer) | author != reviewer)
  
  out$qcers <- rH_qclog %>% dplyr::filter(author == reviewer)
  
  stopifnot(nrow(.repoHistory) == nrow(out$authors) + nrow(out$qcers))
  
  return(out)
}