test_that("svnExport does not work outside of SVN", {
  expect_error(svnExport("script/data-assembly.R"))
})

with_demoRepo({
  svnExport("script/data-assembly.R", 1)
  svnExport("script/combine-da.R", 1)
  svnExport("script/pk/load-spec.R", 3)
  
  test_that("svnExport creates 4 files of past revisions", {
    expect_true(file.exists("data-assembly-1.R"))
    expect_true(file.exists("combine-da-1.R"))
    expect_true(file.exists("load-spec-3.R"))
    
    expect_equal(length(readLines("data-assembly-1.R")), 5)
    expect_equal(length(readLines("combine-da-1.R")), 6)
    expect_equal(readLines("load-spec-3.R"), 
                 "pk_spec <- yspec::load_spec(here::here(\"script\", \"script/examp-yaml.yaml\"))")
  })
  
  svnExport("script/data-assembly.R")
  
  test_that("svnExport works with no previous revision given", {
    expect_true(file.exists("data-assembly-5.R"))
  })
  
  test_that("svnExport doesn't work with incorrect file path", {
    expect_error(svnExport("data-assembly-23.Rmd"))
  })
})

