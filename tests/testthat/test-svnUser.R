repo <- demoRepo("abc-123")

test_that("svnUser does not work with local repository", {
  expect_error(svnUser())
})

file1 <- file.path(repo, "script/data-assembly.R")
examp_info <- svnCommand(.command = "info", .file = file1)

examp_info$entry$repository$root <- "svn+ssh://user1@mc1.metrumrg.com/common/repo/examp-123"
svn_root <- examp_info$entry$repository$root
host_name <- "mc1.metrumrg.com"

svn_ssh_user <- 
  unlist(
    strsplit(
      svn_root,
      split = paste0("@", host_name), 
      fixed = TRUE
    )
  )[1]

test_that("svnUser removes host name as expected", {
  expect_true(svn_ssh_user == "svn+ssh://user1")
})

svn_user <- unlist(strsplit(svn_ssh_user, "svn+ssh://", fixed = TRUE))[2]

test_that("svnUser removes ssh from username", {
  expect_true(svn_user == "user1")
})
