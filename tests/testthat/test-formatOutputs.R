create_test_svn()

df_history <- repoHistory()
df_readOutputs <-
  dplyr::tibble(
    script = c("script/model-summary.Rmd", "script/data-assembly/da-functions.R", 
               "script/data-assembly/da-functions.R", "script/data-assembly/da-study-abc.Rmd"),
    event = "modified",
    output = c("script/test.R", "script/test.R", "script/data-assembly/da-study-abc.Rmd", 
               "script/data-assembly/da-functions.R")
  )

df_format <- formatOutputs(.ro = df_readOutputs, .rh = df_history)

test_that("Correct format data.frame is returned", {
  expect_true(all(names(df_format) %in% c("File", "Output(s)", "File...revision", "Output...revision", "Up to...date?")))
  expect_true(nrow(df_format) == nrow(df_readOutputs))
  expect_true(length(df_format$File[df_format$File == "script/data-assembly/da-functions.R"]) == 2)
})

test_that("Correct status is assigned based on commit history", {
  
  # Cases where file revision comes before or at Output revision are okay
  expect_identical(
    df_format %>% 
      dplyr::filter(File...revision <= Output...revision) %>% 
      dplyr::distinct(`Up to...date?`) %>% 
      dplyr::pull(),
    "\\textcolor{black}{Yes}"
  )
  
  # Flag cases where the output hasn't been checked in since the file was updated
  expect_identical(
    df_format %>% 
      dplyr::filter(File...revision > Output...revision) %>% 
      dplyr::distinct(`Up to...date?`) %>% 
      dplyr::pull(),
    "\\textcolor{red}{No}"
  )
})
