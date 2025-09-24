#' @noRd
getRevHistory <- function(.file) {
  
  if (!file.exists(.file)) {
    stop(paste0("'", .file, "' does not exist."))
  }
  
  .svn_log <- svnLog(.file)

  qclog <- logRead()

  qclog_file <- qclog[qclog$file == .file, ]
  qclog_file <- qclog_file[qclog_file$revf != 0, ]
  .svn_log$datetime <- format(.svn_log$datetime, "%Y-%m-%d %H:%M:%S")

  .svn_log$QCed <- if (nrow(qclog_file) == 0) {
    "No"
  } else {
    ifelse(as.numeric(.svn_log$rev) %in% qclog_file$revf, "Yes", "No")
  }
  
  # Update datetime to relative days
  .svn_log$datetime <- as.numeric(Sys.Date() - as.Date(.svn_log$datetime))
  .svn_log <- 
    .svn_log %>% 
    dplyr::mutate(
      elapsed = dplyr::case_when(
        datetime == 0 ~ "Today",
        datetime == 1 ~ "Yesterday",
        TRUE ~ paste0(datetime, " days ago")
      )
    ) %>% 
    dplyr::select(-datetime)

  .svn_log$rev <- as.numeric(.svn_log$rev)
  .svn_log <- .svn_log[order(-.svn_log$rev), ]
  
  .svn_log$rev <- as.character(.svn_log$rev)
  .svn_log$rev_display <- paste0("Revision: ", .svn_log$rev)
  
  # Add local to svn_log
  local_df <- 
    dplyr::tibble(
      author = Sys.info()[["user"]],
      rev = "Local",
      msg = "Working copy",
      QCed = "No",
      elapsed = "Today",
      rev_display = "Local"
    )
  
  .svn_log <- dplyr::bind_rows(local_df, .svn_log)
  
  .svn_log
}
