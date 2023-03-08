test_dir <- createRepo()
withr::local_dir(test_dir)

# Add path test
system("mkdir script")
system("mkdir data")
add_file("script/file.txt", "soemthing")
add_file("data/filespec.txt", "soemthing")
logCreate()

test_that("relpath outputs the relative path when given an unresolved or absolute path", {
  expect_identical("script/file.txt", pathFromLogRoot("data/../script/file.txt"))
  expect_identical("script/file.txt", relPath("data/../script/file.txt"))
  expect_identical("script/file.txt", pathFromLogRoot("/tmp/svn-testing/test/script/file.txt"))
})

