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

metaList <- svnMeta("file.txt")

test_that("svnMeta includes both log and info meta data", {
  expect_true(all(c("log", "info") %in% names(metaList)))
  expect_equal(length(metaList), 2)
  expect_equal(length(metaList$log), 5)
  expect_equal(length(metaList$info), 1)
})
