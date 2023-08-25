test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")
add_commit("first")

add_file("file.txt", "something2")
add_commit("second")

add_file("file.txt", "something3")
add_commit("third")

add_file("file.txt", "something4")
add_commit("fourth")

add_file("file.txt", "something5")
add_commit("fifth")

metadf <- svnLog("file.txt")

test_that("svnLog includes all commits in dataframe format", {
  expect_true(all(c("author", "date", "msg", "rev") %in% names(metadf)))
  expect_equal(nrow(metadf), 5)
  expect_equal(length(unique(metadf$rev)), 5)
})
