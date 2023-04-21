test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")
add_file("file2.txt", "something with some additions")

diff_data <- diffFiles(.file_1 = "file2.txt", .file_2 = "file.txt")

test_that("diffFiles outputs diff between two different files [REV-DNF-001]", {
  expect_true(diff_data@target != diff_data@current)
  expect_equal(diff_data@target, "something with some additions")
  expect_equal(diff_data@current, "something")
})
