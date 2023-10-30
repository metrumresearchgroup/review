repo <- demoRepo("abc-123")

file1 <- file.path(repo, "script/data-assembly.R")
file2 <- file.path(repo, "script/pk/load-spec.R")

test_that("diffPreviousRevisions outputs diff between two previous specified versions", {
  diffVer <- diffPreviousRevisions(.file = file1, .previous_revision = 1, .current_revision = 5)
  expect_true(diffVer@target[2] != diffVer@current[2])
  expect_equal(diffVer@current[2], "source(here::here(\"script\", \"data-assembly\", \"da-functions.R\"))")
  expect_equal(diffVer@target[1], diffVer@current[1])
  expect_equal(diffVer@current[10], "derived$tv$dosing <- ex_1")
})

test_that("diffPreviousRevisions defaults current version of diff to local version", {
  diffVer <- diffPreviousRevisions(.file = file2, .previous_revision = 1)
  expect_true(diffVer@target != diffVer@current)
  expect_equal(diffVer@target, "pk_spec <- yspec::load_spec(here::here(\"script\", \"script/examp-yaml.yaml\"))")
  expect_equal(diffVer@current, "pk_spec <- yspec::load_spec(here::here(\"script\", \"examp-yaml.yaml\"))")
})