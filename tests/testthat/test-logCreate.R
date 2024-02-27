with_demoRepo({
  file.remove("QClog.csv")
  
  logCreate()
  
  test_that("logCreate generates a csv file", {
    expect_true(file.exists("QClog.csv"))
    expect_true(nrow(readr::read_csv("QClog.csv") %>% suppressMessages()) == 0)
  })
})
