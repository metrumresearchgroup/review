test_that("compareDashboard validates input folder and figure files", {
  tmp_dir <- tempfile("fig-dashboard-")
  dir.create(tmp_dir)

  expect_error(
    compareDashboard(file.path(tmp_dir, "missing")),
    "must be a directory"
  )

  expect_error(
    compareDashboard(tmp_dir),
    "No PDF or PNG figures"
  )
})

test_that("compareDashboard returns a shiny app with expected UI", {
  tmp_dir <- tempfile("fig-dashboard-")
  dir.create(tmp_dir)
  file.create(file.path(tmp_dir, "example.png"))

  app <- compareDashboard(tmp_dir)
  expect_true(inherits(app, "shiny.appobj"))
})
