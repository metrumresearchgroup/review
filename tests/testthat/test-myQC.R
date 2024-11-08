create_test_svn()

logAssign("script/e-appendix/ea-desc.yaml")
logAssign("script/e-appendix/ea-spec.yaml")
logAccept("script/data-assembly/da-combine-studies.Rmd")

res1 <- myQC(.user = "michaelm", .within_days = Inf)

res2 <- myQC(.user = "graceo", .within_days = Inf)

test_that("myQC works as expected", {
  
  expect_equal(length(res1), 2)
  expect_equal(length(res2), 1)
  expect_equal(names(res1), c("Awaiting QC", "Not in QC log"))
  
  expect_true(!any(res1$`Awaiting QC`$File %in% "script/data-assembly/da-combine-studies.Rmd"))
  
  expect_true(nrow(res1$`Awaiting QC`) > 0)
  expect_true(any(res1$`Awaiting QC`$File %in% "script/e-appendix/ea-desc.yaml"))
  
})

