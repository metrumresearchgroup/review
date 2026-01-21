test_that("figureDashboard validates input folder and figure files", {
  tmp_dir <- tempfile("fig-dashboard-")
  dir.create(tmp_dir)

  expect_error(
    figureDashboard(file.path(tmp_dir, "missing")),
    "must be a directory"
  )

  expect_error(
    figureDashboard(tmp_dir),
    "No PDF or PNG figures"
  )
})

test_that("figureDashboard returns a shiny app with expected UI", {
  tmp_dir <- tempfile("fig-dashboard-")
  dir.create(tmp_dir)
  file.create(file.path(tmp_dir, "example.png"))

  app <- figureDashboard(tmp_dir)
  expect_true(inherits(app, "shiny.appobj"))
})
