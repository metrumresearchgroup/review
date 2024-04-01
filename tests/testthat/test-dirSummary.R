with_demoRepo({
  dirSummaryRes <- dirSummary()
  dirSummaryResExcl <- dirSummary(.dirs_exclude = "script/pk")
  
  # Check that QC summary contains expected information
  test_that("dirSummary returns the correct project name", {
    expect_equal(basename(logRoot()), dirSummaryRes$project)
  })
  
  test_that("dirSummary returns all expected output", {
    expect_equal(c("project", "data", "status"), names(dirSummaryRes))
    expect_true(length(dirSummaryRes) == 3)
    expect_true(is.character(dirSummaryRes$project))
    expect_true(is.data.frame(dirSummaryRes$data))
    expect_equal(nrow(dirSummaryRes$data), 4)
    expect_true(is.data.frame(dirSummaryRes$status))
    expect_equal(nrow(dirSummaryRes$status), 4)
    expect_true(length(dirSummaryResExcl) == 3)
    expect_equal(nrow(dirSummaryResExcl$data), 3)
    expect_equal(nrow(dirSummaryResExcl$status), 3)
  })
  
  test_that("dirSummary captures the expected QC status of all scripts", {
    
    expect_equal(
      "script/combine-da.R",
      dirSummaryRes$data %>% dplyr::filter(Status == "QC up to date") %>% dplyr::pull(File)
    )
    
    # Latest rev
    expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/combine-da.R") %>% dplyr::pull(`Latest rev`) == "1")
    expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/data-assembly.R") %>% dplyr::pull(`Latest rev`) == "5")
    expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/examp-yaml.yaml") %>% dplyr::pull(`Latest rev`) == "1")
    
    # Status
    expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/combine-da.R") %>% dplyr::pull(Status) == "QC up to date")
    expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/data-assembly.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
    expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/examp-yaml.yaml") %>% dplyr::pull(Status) == "Not in QC log")
    expect_true(dirSummaryRes$data %>% dplyr::filter(File == "script/pk/load-spec.R") %>% dplyr::pull(Status) == "In QC log, needs QC")
    
    # Date
    expect_true(
      abs(as.numeric(
        difftime(
          format(Sys.time(), tz = "UTC"),
          format(dirSummaryRes$data$`Latest edit`[1], tz = "UTC"),
          units = "secs")
      )) < 120
    )
    
    # Author
    expect_equal(dirSummaryRes$data$Author[1], Sys.info()[["user"]])
    
  })
  
  # QC Status data.frame
  
  test_that("dirSummary generates formatted dataframe of QC status", {
    expect_true(is.factor(dirSummaryRes$status$Status))
    expect_true(nrow(dirSummaryRes$status %>% dplyr::filter(Status == "QC up to date")) == 1)
    expect_true(nrow(dirSummaryRes$status %>% dplyr::filter(Status == "In QC log, needs QC")) == 2)
    expect_true(nrow(dirSummaryRes$status %>% dplyr::filter(Status == "Not in QC log")) == 1)
  })
})

