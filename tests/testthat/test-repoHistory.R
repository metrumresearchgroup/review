create_test_svn()

df_history <- repoHistory()

test_that("repoHistory works as expected", {
  
  expect_equal(nrow(df_history %>% dplyr::distinct(rev)), 8)
  
  expect_true(nrow(df_history %>% dplyr::distinct(author)) > 1)
  
  expect_true(inherits(df_history$msg, "character"))
  expect_true(inherits(df_history$author, "character"))
  expect_true(inherits(df_history$file, "character"))
  expect_true(inherits(df_history$rev, "character"))
  expect_true(inherits(df_history$date, "Date"))
  expect_true(grepl("script", df_history$file[1]))
  
})

