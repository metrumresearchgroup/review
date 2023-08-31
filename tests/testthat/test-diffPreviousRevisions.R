test_dir <- createRepo()
withr::local_dir(test_dir)

add_file("file.txt", "something")

logCreate()

add_commit("first")
logAssign(file = "file.txt")
logAccept(file = "file.txt")

add_file("file.txt", "something2")
add_commit("second")
logAccept(file = "file.txt")

add_file("file.txt", "something3")
add_commit("third")
logAccept(file = "file.txt")

add_file("file.txt", "something4")
add_commit("fourth")
logAccept(file = "file.txt")

add_file("file.txt", "something5")

test_that("diffPreviousRevisions outputs diff between two previous specified versions", {
  diffVer <- diffPreviousRevisions(.file = "file.txt", .previous_revision = 2, .current_revision = 4)
  expect_true(diffVer@target != diffVer@current)
  expect_equal(diffVer@target, "something2")
  expect_equal(diffVer@current, "something4")
})

test_that("diffPreviousRevisions defaults current version of diff to local version", {
  diffVer <- diffPreviousRevisions(.file = "file.txt", .previous_revision = 2)
  expect_true(diffVer@target != diffVer@current)
  expect_equal(diffVer@target, "something2")
  expect_equal(diffVer@current, "something5")
  
  diffVer <- diffPreviousRevisions(.file = "file.txt", .previous_revision = 1)
  expect_true(diffVer@target != diffVer@current)
  expect_equal(diffVer@target, "something")
})
