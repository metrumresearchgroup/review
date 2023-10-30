repo <- demoRepo("abc-123")
setwd(repo)

test_that("logAccept creates a row in the QClog for the specified file", {
  logAccept("script/data-assembly.R")
  logAccept("script/examp-txt.txt")
  logAccept("script/pk/load-spec.R")
  
  tempqc <- logRead() %>% dplyr::arrange(-revf)
  
  expect_equal(
    tempqc %>% dplyr::filter(file == "script/data-assembly.R") %>% dplyr::slice(1) %>% dplyr::pull(revf),
    5)
  
  expect_equal(
    tempqc %>% dplyr::filter(file == "script/examp-txt.txt") %>% dplyr::slice(1) %>% dplyr::pull(revf),
    1)
  
  expect_equal(
    tempqc %>% dplyr::filter(file == "script/pk/load-spec.R") %>% dplyr::slice(1) %>% dplyr::pull(revf),
    4)
})
