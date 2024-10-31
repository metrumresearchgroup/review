create_test_svn()

review::logAccept("script/data-assembly/da-functions.R")

qclog <- logRead()
df_history <- repoHistory()

authors_qcers <- findAuthorsQcers(.repoHistory = df_history, .qcLog = qclog)

test_that("findAuthorsQcers works as expected", {
  
  expect_equal(length(authors_qcers), 2)
  expect_equal(names(authors_qcers), c("authors", "qcers"))
  expect_true(inherits(authors_qcers, "list"))
  expect_true(inherits(authors_qcers$authors, "data.frame"))
  
  expect_true(all(authors_qcers$qcers$author == authors_qcers$qcers$reviewer))
  expect_true(all(is.na(authors_qcers$authors$reviewer) | 
                    authors_qcers$authors$author != authors_qcers$authors$reviewer))
  
  expect_true(nrow(authors_qcers$authors %>% dplyr::distinct(file, rev)) == nrow(authors_qcers$authors))

})
