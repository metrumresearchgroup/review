test_that("logAccept creates a row in the QClog for the specified file", {
  
  with_demoRepo({
    logAccept("script/data-assembly.R")
    logAccept("script/examp-txt.txt")
    logAccept("script/pk/load-spec.R")
    
    tempqc <- logRead() %>% dplyr::arrange(-revf)
    
    expect_equal(
      tempqc %>%
        dplyr::filter(file == "script/data-assembly.R") %>%
        dplyr::slice(1) %>%
        dplyr::pull(revf),
      5
    )
    
    expect_equal(
      tempqc %>% dplyr::filter(file == "script/examp-txt.txt") %>% dplyr::slice(1) %>% dplyr::pull(revf),
      1
    )
    
    expect_equal(
      tempqc %>% dplyr::filter(file == "script/pk/load-spec.R") %>% dplyr::slice(1) %>% dplyr::pull(revf),
      4
    )
  })
})

test_that("logAccept requires a prior log entry for every file", {
  with_demoRepo({
    file <- "script/examp-yaml.yaml"
    old <- logRead()
    logWrite(old[old$file != file, ], logName())
    before <- logRead()
    expect_error(
      logAccept(c("script/data-assembly.R", file)),
      "Run logAssign() before logAccept()", fixed = TRUE
    )
    expect_equal(logRead(), before)
    logAssign(file)
    expect_error(logAccept(file), NA)
  })
})

test_that("logAccept accepts files with only nonzero revision history", {
  with_demoRepo({
    file <- "script/data-assembly.R"
    logAccept(file)
    old <- logRead()
    logWrite(old[old$file != file | old$revf != 0, ], logName())
    expect_true(all(logRead()$revf[logRead()$file == file] > 0))
    expect_error(logAccept(file), NA)
  })
})
