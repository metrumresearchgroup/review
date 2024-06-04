# Developer sys and svn info
user_lookup <- dplyr::tribble(
  ~sys, ~svn,
  "michaelm", "michaelm",
  "anderson", "andersone",
  "graceo", "graceo"
)

create_test_svn <- function(.user_lookup = user_lookup) {
  
  this_user <- 
    .user_lookup %>% 
    dplyr::filter(sys == Sys.info()[["user"]]) %>% 
    dplyr::pull(svn)
  
  testthat::skip_if(
    length(this_user) == 0,
    glue::glue("Skipped svnUser tests because user not found in svn lookup in setup.R")
  )
  
  remote_repo_local <- paste0(tempdir(), "/test")
  
  .command <-
    glue::glue("svn co svn+ssh://{this_user}@mc1-test.metrumrg.com/common/repo/svn-proj-review-tests {remote_repo_local} -q -q")
  
  system(.command)
  
  setwd(remote_repo_local)
}

