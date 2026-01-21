test_that("figureDashboard validates inputs and returns a shiny app", {
  tmp_dir <- tempfile("fig-dashboard-")
  dir.create(tmp_dir)
  file.create(file.path(tmp_dir, "example.png"))

  app <- figureDashboard(tmp_dir)
  expect_true(inherits(app, "shiny.appobj"))

  expect_error(figureDashboard(file.path(tmp_dir, "missing")))
})
