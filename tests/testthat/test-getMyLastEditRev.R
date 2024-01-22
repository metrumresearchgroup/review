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

# Create generalized tests that will pass independent of who is running them
file1 <- "script/data-assembly/da-combine-studies.Rmd"

svnLog1 <- svnLog(file1)

if (user_res$svn %in% svnLog1$author) {
  last_rev1 <-
    svnLog1 %>% 
    dplyr::filter(author == user_res$svn) %>% 
    dplyr::slice(1)
  
  testthat::test_that("getMyLastEditRev finds the correct revision the user last made an edit", {
    expect_equal(as.numeric(last_rev1$rev), getMyLastEditRev(file1, .host_name = "mc1-test.metrumrg.com")[["revision"]])
    
    expect_equal(user_res$svn, getMyLastEditRev(file1, .host_name = "mc1-test.metrumrg.com")[["usernameSVN"]])
    expect_equal(last_rev1$datetime, getMyLastEditRev(file1, .host_name = "mc1-test.metrumrg.com")[["datetime"]])
  })
}

# Create generalized tests that will pass independent of who is running them
file2 <- "script/data-assembly/da-functions.R"

svnLog2 <- svnLog(file2)

if (user_res$svn %in% svnLog2$author) {
  last_rev2 <-
    svnLog2 %>% 
    dplyr::filter(author == user_res$svn) %>% 
    dplyr::slice(1)
  
  testthat::test_that("getMyLastEditRev finds the correct revision the user last made an edit", {
    expect_equal(as.numeric(last_rev2$rev), getMyLastEditRev(file2, .host_name = "mc1-test.metrumrg.com")[["revision"]])
    
    expect_equal(last_rev2$datetime, getMyLastEditRev(file2, .host_name = "mc1-test.metrumrg.com")[["datetime"]])
  })
}

file3 <- "script/data-assembly/da-study-abc.Rmd"

svnLog3 <- svnLog(file3)

if (user_res$svn %in% svnLog2$author) {
  last_rev3 <-
    svnLog3 %>% 
    dplyr::filter(author == user_res$svn) %>% 
    dplyr::slice(1)
  
  testthat::test_that("getMyLastEditRev finds the correct revision the user last made an edit", {
    expect_equal(as.numeric(last_rev3$rev), getMyLastEditRev(file3, .host_name = "mc1-test.metrumrg.com")[["revision"]])
    
    expect_equal(last_rev3$datetime, getMyLastEditRev(file3, .host_name = "mc1-test.metrumrg.com")[["datetime"]])
  })
}
