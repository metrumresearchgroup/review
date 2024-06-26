test_that("svnList does not work outside of SVN", {
  expect_error(svnProjInfo())
})

create_test_svn()

proj_res <- svnProjInfo(.host_name = "mc1-test.metrumrg.com")

this_user <- 
  user_lookup %>% 
  dplyr::filter(sys == Sys.info()[["user"]]) %>% 
  dplyr::pull(svn)

test_that("svnUser retrieves correct svn user", {
  expect_true(proj_res$this_svn_user == this_user)
})
