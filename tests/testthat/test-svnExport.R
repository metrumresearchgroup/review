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

svnExport("file.txt", 1)
svnExport("file.txt", 2)
svnExport("file.txt", 3)
svnExport("file.txt", 4)

test_that("svnExport creates 4 files of past revisions", {
  expect_true(file.exists("file-1.txt"))
  expect_true(file.exists("file-2.txt"))
  expect_true(file.exists("file-3.txt"))
  expect_true(file.exists("file-4.txt"))
  
  expect_equal(readLines("file-1.txt"), "something")
  expect_equal(readLines("file-2.txt"), "something2")
  expect_equal(readLines("file-3.txt"), "something3")
  expect_equal(readLines("file-4.txt"), "something4")
})
