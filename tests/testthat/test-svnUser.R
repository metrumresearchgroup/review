this_user <- 
  user_lookup %>% 
  dplyr::filter(sys == Sys.info()[["user"]]) %>% 
  dplyr::pull(svn)

testthat::skip_if(
  length(this_user) == 0,
  glue::glue("Skipped svnUser tests because user not found in svn lookup in setup.R")
)

remote_repo_local <- paste0(tempdir(), "/test")

.command <-
  glue::glue("svn co svn+ssh://{this_user}@mc1-test.metrumrg.com/common/repo/svn-proj-review-tests {remote_repo_local}")

system(.command)

setwd(remote_repo_local)

user_res <- svnUser(.host_name = "mc1-test.metrumrg.com")

