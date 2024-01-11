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
  glue::glue("svn co svn+ssh://{this_user}@mc1-test.metrumrg.com/common/repo/svn-proj-review-tests {remote_repo_local} -q -q")

system(.command)

setwd(remote_repo_local)

user_res <- svnUser(.host_name = "mc1-test.metrumrg.com")


test_that("svnUser retrieves correct system user", {
  expect_true(user_res$sys == Sys.info()[["user"]])
})

test_that("svnUser retrieves correct svn user", {
  expect_true(user_res$svn == this_user)
})
