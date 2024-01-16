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

last_rev1 <-
  svnLog(file1) %>% 
  dplyr::filter(author == user_res$svn) %>% 
  dplyr::slice(1)

if (nrow(last_rev1) == 1) {
  testthat::test_that("diffSinceMyLastEdit gives expected result for file1", {
    diffSincefunc <- diffSinceMyLastEdit(file1, .host_name = "mc1-test.metrumrg.com") %>% suppressMessages()
    diffPrevfunc <- diffPreviousRevisions(.file = file1, .previous_revision = as.numeric(last_rev1$rev))
    
    if (is.null(diffSincefunc)) {
      expect_equal(
        diffSinceMyLastEdit(file1, .host_name = "mc1-test.metrumrg.com") %>% suppressMessages(),
        diffPreviousRevisions(.file = file1, .previous_revision = as.numeric(last_rev1$rev))
      )
    } else {
      expect_equal(
        diffSincefunc@tar.dat$orig,
        diffPrevfunc@tar.dat$orig
      )
    }
  })
}

file2 <- "script/data-assembly/da-functions.R"

last_rev2 <-
  svnLog(file2) %>% 
  dplyr::filter(author == user_res$svn) %>% 
  dplyr::slice(1)

if (nrow(last_rev2) == 1) {
  testthat::test_that("diffSinceMyLastEdit gives expected result for file2", {
    diffSincefunc <- diffSinceMyLastEdit(file2, .host_name = "mc1-test.metrumrg.com") %>% suppressMessages()
    diffPrevfunc <- diffPreviousRevisions(.file = file2, .previous_revision = as.numeric(last_rev2$rev))
    
    if (is.null(diffSincefunc)) {
      expect_equal(
        diffSinceMyLastEdit(file2, .host_name = "mc1-test.metrumrg.com") %>% suppressMessages(),
        diffPreviousRevisions(.file = file2, .previous_revision = as.numeric(last_rev2$rev))
      )
    } else {
      expect_equal(
        diffSincefunc@tar.dat$orig,
        diffPrevfunc@tar.dat$orig
      )
    }
  })
}

file3 <- "script/data-assembly/da-study-abc.Rmd"

last_rev3 <-
  svnLog(file3) %>% 
  dplyr::filter(author == user_res$svn) %>% 
  dplyr::slice(1)

if (nrow(last_rev3) == 1) {
  testthat::test_that("diffSinceMyLastEdit gives expected result for file2", {
    diffSincefunc <- diffSinceMyLastEdit(file3, .host_name = "mc1-test.metrumrg.com") %>% suppressMessages()
    diffPrevfunc <- diffPreviousRevisions(.file = file3, .previous_revision = as.numeric(last_rev3$rev))
    
    if (is.null(diffSincefunc)) {
      expect_equal(
        diffSinceMyLastEdit(file3, .host_name = "mc1-test.metrumrg.com") %>% suppressMessages(),
        diffPreviousRevisions(.file = file3, .previous_revision = as.numeric(last_rev3$rev))
      )
    } else {
      expect_equal(
        diffSincefunc@tar.dat$orig,
        diffPrevfunc@tar.dat$orig
      )
    }
  })
}
