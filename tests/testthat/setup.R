# Force sequential execution so tests match CI behaviour and don't spawn extra workers.
future::plan("sequential")

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
  
  checkout_status <- system(.command)
  testthat::skip_if(
    checkout_status != 0 || !dir.exists(remote_repo_local),
    "Skipped SVN integration tests because the test repository could not be checked out"
  )
  
  setwd(remote_repo_local)
}
