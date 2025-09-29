test_that("show_app_exit_hint returns invisibly", {
  # Should not print a visible value
  expect_invisible(show_app_exit_hint("MyApp"))
})

test_that("app name is interpolated into the output", {
  
  # Capture messages so we can grep for the app name
  msgs <- testthat::capture_messages(show_app_exit_hint("CoolApp"))
  # The app name should appear at least in the rule and list item
  expect_true(any(grepl("Launching CoolApp", msgs, fixed = TRUE)))
  expect_true(any(grepl("running CoolApp", msgs, fixed = TRUE)))
})
