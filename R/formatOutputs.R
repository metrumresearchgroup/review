#' Combine outputs and repo history information
#' 
#' @description
#' Combines information from readOutputs() and repoHistory()
#' to create a data.frame to be put into a table in
#' renderOutputs().
#' 
#' @param .ro Output of readOutputs()
#' @param .rh Output of repoHistory()
#' 
#' @noRd
formatOutputs <- function(.ro = readOutputs(), .rh = repoHistory()) {
  
  .readOutputs <- .ro
  .repoHistory <- .rh
  
  # Update format of the outputs
  df_outputs <-
    .readOutputs %>% 
    dplyr::rename(File = script, `Output(s)` = output) %>% 
    dplyr::select(-event)
  
  # Grab latest commit from each file
  rh2 <-
    .repoHistory %>% 
    dplyr::group_by(file) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # Format repo history for join with source file
  rh3a <-
    rh2 %>% 
    dplyr::transmute(
      File = file,
      `File...revision` = rev
    )
  
  # Format repo history for join with outputs
  rh3b <-
    rh2 %>% 
    dplyr::transmute(
      `Output(s)` = file,
      `Output...revision` = rev
    )
  
  # Combine revision history with outputs
  df_outputs2 <-
    df_outputs %>% 
    dplyr::left_join(rh3a) %>% 
    dplyr::left_join(rh3b)
  
  # Add a "Up to date" column to the dataframe
  df_outputs3 <-
    df_outputs2 %>% 
    # Assign file actions
    dplyr::mutate(
      `Up to...date?` = dplyr::case_when(
        is.na(Output...revision) ~ "Not in SVN",
        `File...revision` > `Output...revision` ~ "\\textcolor{red}{No}",
        TRUE ~ "\\textcolor{black}{Yes}"
      )
    )
  
  df_outputs3
  
}
