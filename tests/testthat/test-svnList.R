create_test_svn()

svn_list <- svnList()

test_that("svnList works for standard case", {
  expect_true(all(file.exists(svn_list$file)))
  expect_true(nrow(svn_list %>% dplyr::count(rev)) > 1)
  expect_true(!any(grepl("renv", svn_list$file)))
})
