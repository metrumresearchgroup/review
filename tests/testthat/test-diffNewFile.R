test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")
add_file("file2.txt", "something with some additions")

diff_data <- diffNewFile("file2.txt", "file.txt")

test_that("diffNewFile outputs diff between two different files [REV-DNF-001]", {
  expect_true(diff_data@target != diff_data@current)
  expect_equal(diff_data@target, "something")
  expect_equal(diff_data@current, "something with some additions")
})
