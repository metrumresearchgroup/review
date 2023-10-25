test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")
add_commit("first")

add_file("file.txt", "something2")
add_commit("second")

svn_info <- svnInfo("file.txt")
svn_log <- svnLog("file.txt")

test_that("svnInfo the last commit in dataframe firnat", {
  expect_true(all(c("author", "datetime", "rev") %in% names(svn_info)))
  expect_equal(nrow(svn_info), 1)
})

test_that("svnInfo matches the first row of svnLog for the columns they have in common", {
  expect_true(identical(svn_info, svn_log[1, c("author", "datetime", "rev")]))
})
