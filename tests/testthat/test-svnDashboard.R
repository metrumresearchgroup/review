test_that("svnDashboard validates its input directory", {
  expect_error(
    svnDashboard(file.path(tempdir(), "missing-working-copy")),
    "must be an SVN working-copy directory"
  )
})

test_that("svnDashboard returns a Shiny app containing SVN status", {
  local_mocked_bindings(
    svnStatus = function(.file, .relative_to = NULL, .show_updates = FALSE) {
      data.frame(path = "analysis.R", status = "modified")
    },
    .package = "review"
  )

  app <- svnDashboard(tempdir())

  expect_s3_class(app, "shiny.appobj")
})

test_that("svnDashboard shows a clean-working-copy state", {
  local_mocked_bindings(
    svnStatus = function(.file, .relative_to = NULL, .show_updates = FALSE) {
      data.frame(path = character(), status = character())
    },
    .package = "review"
  )

  app <- svnDashboard(tempdir())

  expect_s3_class(app, "shiny.appobj")
})

test_that("SVN status display uses canonical symbols and Paul Tol colors", {
  status <- decorateSvnStatus(data.frame(
    path = c("modified.R", "deleted.R", "new.R"),
    status = c("modified", "deleted", "unversioned"),
    out_of_date = c(FALSE, TRUE, FALSE),
    remote_status = c(NA, "modified", NA)
  ))

  expect_identical(status$symbol, c("M", "D", "?"))
  expect_identical(status$color, c("#CCBB44", "#EE6677", "#66CCEE"))
  expect_identical(status$remote_symbol, c("-", "*", "-"))
  expect_identical(status$remote_color, c("#BBBBBB", "#4477AA", "#BBBBBB"))
})

test_that("dashboard commands are formatted exactly and quote spaces", {
  command <- dashboardCommand(
    command = "svn",
    args = c("commit", "-m", "explain change", "--", "script/a b.R"),
    cwd = tempdir(),
    title = "Commit",
    effect = "Commit one file",
    risk = "repository",
    paths = "script/a b.R",
    expected_status = c("script/a b.R" = "modified")
  )

  expect_identical(
    formatDashboardCommand(command),
    "svn commit -m 'explain change' -- 'script/a b.R'"
  )
})

test_that("dashboard commands are blocked when previewed status is stale", {
  command <- dashboardCommand(
    command = "svn",
    args = c("add", "--", "new.R"),
    cwd = tempdir(),
    title = "Add",
    effect = "Add one file",
    risk = "normal",
    paths = "new.R",
    expected_status = c("new.R" = "unversioned")
  )

  expect_true(dashboardStatusIsCurrent(
    command,
    data.frame(path = "new.R", status = "unversioned")
  ))
  expect_false(dashboardStatusIsCurrent(
    command,
    data.frame(path = "new.R", status = "added")
  ))
  expect_false(dashboardStatusIsCurrent(
    command,
    data.frame(path = character(), status = character())
  ))
})

test_that("dashboard command runner captures SVN output", {
  skip_if(Sys.which("svn") == "")
  command <- dashboardCommand(
    command = "svn",
    args = c("--version", "--quiet"),
    cwd = tempdir(),
    title = "Version",
    effect = "Read SVN version",
    risk = "normal",
    paths = character(),
    expected_status = character()
  )

  result <- runDashboardCommand(command)

  expect_identical(result$status, 0L)
  expect_match(result$stdout, "^[0-9]+\\.[0-9]+")
})

test_that("selected SVN statuses expose the appropriate actions", {
  local_mocked_bindings(
    svnStatus = function(.file, .relative_to = NULL, .show_updates = FALSE) {
      data.frame(
        path = c("modified.R", "missing.R", "new.R"),
        status = c("modified", "missing", "unversioned"),
        remote_status = c(NA_character_, NA_character_, NA_character_),
        out_of_date = c(FALSE, FALSE, FALSE)
      )
    },
    show_app_exit_hint = function(...) invisible(),
    .package = "review"
  )

  app <- svnDashboard(tempdir())
  shiny::testServer(app$serverFuncSource(), {
    expect_true(any(grepl("All listed files", output$selection_controls, fixed = TRUE)))
    expect_true(any(grepl("Modified (M)", output$selection_controls, fixed = TRUE)))
    expect_true(any(grepl("Missing locally (!)", output$selection_controls, fixed = TRUE)))
    expect_true(any(grepl("Untracked (?)", output$selection_controls, fixed = TRUE)))
    expect_true(any(grepl("Clear selection", output$selection_controls, fixed = TRUE)))

    session$setInputs(status_selection = "new.R")
    session$flushReact()
    expect_true(any(grepl("Add selected", output$selection_actions, fixed = TRUE)))
    expect_true(any(grepl("Delete selected locally", output$selection_actions, fixed = TRUE)))

    session$setInputs(status_selection = "modified.R")
    session$flushReact()
    expect_true(any(grepl("Schedule SVN deletion", output$selection_actions, fixed = TRUE)))
    expect_true(any(grepl("Commit selected", output$selection_actions, fixed = TRUE)))

    session$setInputs(status_selection = "missing.R")
    session$flushReact()
    expect_true(any(grepl("Schedule SVN deletion", output$selection_actions, fixed = TRUE)))
    expect_false(any(grepl("Commit selected", output$selection_actions, fixed = TRUE)))
  })
})
