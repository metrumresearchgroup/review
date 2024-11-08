#' Outstanding QC check by user
#'
#' @description
#' This function checks the QC status of files last authored by the given user in a repository.
#' The information provided includes if a file is awaiting QC or not present in the QC log.
#' Users can also filter the results by days since their last edit, to only look at files edited
#' within a specified period of time.
#'
#' @param .user A character string indicating the user whose commit history is being examined. Default is the current system user, determined by `Sys.info()[["user"]]`.
#' @param .within_days A numeric value indicating how many days back to look for edits.
#'   Default is `Inf`, meaning no time limit is applied.
#'
#' @return A list of data frames, each representing a set of files with the same QC status. The data frames include the following columns:
#'   - `File`: The name of the file.
#'   - `Day(s) since last edit`: The number of days since the last modification.
#'
#' @examples
#' \dontrun{
#' # Call myQC for the current user and look at files edited within the last 14 days
#' result <- myQC(.within_days = 14)
#'
#' # Print the QC status for files that are awaiting QC
#' print(result$`Awaiting QC`)
#' }
#'
#' @export
myQC <- function(.user = Sys.info()[["user"]],
                 .within_days = Inf) {
  rH <- repoHistory()
  qclog <- logRead()
  
  qcRev <- 
  if (nrow(qclog) == 0) {
    
    message("QC log is empty")
    dplyr::tibble(file = NA_character_, QCREV = NA_real_)
    
  } else {
    
      qclog %>%
      dplyr::mutate(rev = as.numeric(revf)) %>%
      dplyr::group_by(file) %>%
      dplyr::filter(rev == max(rev)) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(file, QCREV = rev) %>% 
      dplyr::distinct()
    
  }
  
  relevant_file_types <-
    c("R",
      "Rmd",
      "yaml",
      "yml",
      "ctl",
      "cpp",
      "cp",
      "mod",
      "stan",
      "jl",
      "qmd")
  
  rH2 <-
    rH %>%
    dplyr::filter(tools::file_ext(file) %in% relevant_file_types) %>%
    dplyr::filter(grepl("script/", file, fixed = TRUE)) %>%
    dplyr::mutate(rev = as.numeric(rev)) %>%
    dplyr::group_by(file) %>%
    dplyr::filter(rev == max(rev)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(author == .user)
  
  rH3 <-
    rH2 %>%
    dplyr::mutate(INLOG = dplyr::if_else(file %in% qclog$file, 1, 0)) %>%
    dplyr::left_join(qcRev, by = "file") %>%
    dplyr::mutate(QCREV = tidyr::replace_na(QCREV, 0)) %>%
    dplyr::filter(rev != QCREV)
  
  rH4 <-
    rH3 %>%
    dplyr::transmute(
      File = file,
      `Day(s) since last edit` = as.numeric(difftime(Sys.Date(), date, units = "days")),
      QCSTATUS = dplyr::case_when(
        INLOG == 0 ~ "Not in QC log",
        TRUE ~ "Awaiting QC"
      )
    ) %>%
    dplyr::arrange(QCSTATUS, `Day(s) since last edit`) %>%
    dplyr::filter(`Day(s) since last edit` <= .within_days)
  
  rH5 <-
    rH4 %>%
    dplyr::group_by(QCSTATUS) %>%
    dplyr::select(`Day(s) since last edit`, File, QCSTATUS) %>%
    dplyr::group_split() %>% 
    stats::setNames(unique(rH4$QCSTATUS)) %>%
    lapply(function(x) {
      as.data.frame(dplyr::select(x,-QCSTATUS))
    })
  
  rH5
  
}