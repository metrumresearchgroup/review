local_review_repo <- function(.local_envir = parent.frame()) {
  work <- withr::local_tempdir(.local_envir = .local_envir)
  repo <- fs::path(work, "repo")
  wc <- fs::path(work, "working copy")
  processx::run("svnadmin", c("create", repo))
  svnRun("checkout", paste0("file://", repo), wc)
  withr::local_dir(wc, .local_envir = .local_envir)
  fs::dir_create("script")
  writeLines("# analysis", "script/analysis.R")
  writeLines("# other", "script/other.R")
  logCreate()
  svnRun("add", ".", "--force")
  svnRun("commit", "-m", "initial files")
  testthat::local_mocked_bindings(
    review_user = function() "bob", .package = "review",
    .env = .local_envir
  )
  invisible(wc)
}

test_that("ready assigns new files once and records quoted comments losslessly", {
  local_review_repo()
  note <- 'Please check this, including "quotes".\nSecond line: café.'
  expect_message(
    event <- logReady("script/analysis.R", reviewer = "alice", comment = note),
    "tagged alice"
  )
  expect_identical(event$author, "bob")
  expect_identical(event$reviewer, "alice")
  expect_identical(event$sender, "bob")
  expect_equal(event$revision, 1)
  expect_match(event$time, " GMT$")
  qc <- logRead()
  expect_equal(nrow(qc), 1)
  expect_identical(qc$reviewer, "alice")
  expect_equal(qc$revf, 0)
  expect_identical(reviewHistory("script/analysis.R")$comment, note)
  logReady("script/analysis.R")
  expect_equal(nrow(logRead()), 1)
  expect_true(is.na(reviewHistory()$comment[[2]]))
})

test_that("handoffs reuse roles across repeated feedback and final acceptance", {
  local_review_repo()
  file <- "script/analysis.R"
  logReady(file, reviewer = "alice")
  expect_equal(nrow(reviewPending("alice")), 1)
  logComments(file, comment = "Check missing values")
  expect_equal(nrow(reviewPending("alice")), 0)
  expect_identical(reviewPending("bob")$action, "comments")
  logAddressed(file, comment = "Handled missing values")
  logComments(file, comment = "Please also check zeros")
  logAddressed(file, comment = "Handled zeros")
  logResolved(file, comment = "Remove comments")
  expect_identical(reviewPending("bob")$action, "resolved")
  logReady(file, comment = "Cleaned up")
  expect_identical(reviewPending("alice")$action, "ready")
  logAccept(file, reviewer = "alice")
  history <- reviewHistory(file)
  expect_identical(history$action, c(
    "ready", "comments", "addressed", "comments", "addressed",
    "resolved", "ready", "accepted"
  ))
  expect_true(all(history$author == "bob"))
  expect_true(all(history$reviewer == "alice"))
  expect_equal(nrow(reviewPending("bob")), 0)
  expect_equal(nrow(reviewPending("alice")), 0)
  expect_equal(nrow(logPending()), 0)
  logReady(file)
  expect_identical(reviewPending("alice")$action, "ready")
})

test_that("explicit recipients update the pairing and files remain independent", {
  local_review_repo()
  file <- "script/analysis.R"
  logReady(file, reviewer = "alice")
  logComments(file, author = "carol")
  logAddressed(file, reviewer = "dave")
  logResolved(file)
  expect_identical(reviewPending("carol")$recipient, "carol")
  logReady(file)
  expect_identical(reviewPending("dave")$reviewer, "dave")
  expect_identical(reviewPending("dave")$author, "carol")
  logReady("script/other.R", reviewer = "eve")
  expect_equal(nrow(reviewHistory(file)), 5)
  expect_identical(reviewPending("eve")$file, "script/other.R")
})

test_that("the picker uses QC and history usernames and permits manual usernames", {
  local_review_repo()
  logAssign("script/analysis.R", reviewer = "alice")
  logAssign("script/other.R")
  logReady("script/analysis.R", reviewer = "carol")
  local_mocked_bindings(
    review_choose_user = function(users, role) {
      expect_identical(users, c("alice", "bob", "carol"))
      expect_identical(role, "reviewer")
      "new-user"
    }, .package = "review"
  )
  logReady("script/other.R")
  expect_identical(reviewHistory("script/other.R")$reviewer, "new-user")
})

test_that("unknown recipients require explicit usernames outside interactive use", {
  local_review_repo()
  expect_error(logReady("script/analysis.R"), "Supply 'reviewer' explicitly")
  expect_error(logComments("script/analysis.R"), "Supply 'author' explicitly")
  expect_equal(nrow(logRead()), 0)
  expect_equal(nrow(reviewHistory()), 0)
  logComments("script/analysis.R", author = "carol")
  expect_identical(reviewHistory()$reviewer, "bob")
  expect_identical(reviewHistory()$author, "carol")
})

test_that("paths work from subdirectories, absolute paths, and directory arguments", {
  root <- local_review_repo()
  logReady(fs::path(root, "script/analysis.R"), reviewer = "alice")
  withr::with_dir("script", {
    logAddressed("./analysis.R")
    expect_equal(nrow(reviewHistory("analysis.R")), 2)
    expect_identical(reviewPending("alice")$file, "script/analysis.R")
  })
  withr::with_dir(tempdir(), {
    logResolved("analysis.R", directory = fs::path(root, "script"))
    expect_equal(nrow(reviewHistory(directory = root)), 3)
  })
  expect_identical(getwd(), as.character(root))
})

test_that("bad input fails before creating assignments or handoffs", {
  root <- local_review_repo()
  file <- "script/analysis.R"
  expect_error(logReady("missing.R", reviewer = "alice"), "does not exist")
  expect_error(logReady(c(file, file), reviewer = "alice"), "single")
  expect_error(logReady("../outside.R", reviewer = "alice"), "within")
  expect_error(logReady(file, reviewer = "anyone"), "named reviewer")
  expect_error(logReady(file, reviewer = NA_character_), "single")
  expect_error(logReady(file, reviewer = "alice", comment = c("a", "b")), "single")
  writeLines("# local", "script/local.R")
  expect_warning(
    expect_error(logReady("script/local.R", reviewer = "alice"), "tracked in SVN"),
    "svn info"
  )
  expect_equal(nrow(logRead()), 0)
  expect_equal(nrow(reviewHistory()), 0)
  expect_identical(getwd(), as.character(root))
  withr::with_dir(withr::local_tempdir(), {
    expect_error(logReady(file, reviewer = "alice"), "logCreate")
  })
})

test_that("acceptance preserves legacy behavior and supports multiple files", {
  local_review_repo()
  files <- c("script/analysis.R", "script/other.R")
  expect_equal(logAccept(files, reviewer = "alice"), 2)
  expect_false(file.exists("Reviewlog.csv"))
  logReady(files[[1]], reviewer = "alice")
  logReady(files[[2]], reviewer = "carol")
  expect_equal(logAccept(files, reviewer = c("alice", "carol")), 2)
  expect_equal(nrow(reviewPending("alice")), 0)
  expect_equal(nrow(reviewPending("carol")), 0)
  accepted <- dplyr::filter(reviewHistory(), action == "accepted")
  expect_identical(accepted$sender, c("alice", "carol"))
})


test_that("renaming a file preserves its pairing and review history", {
  local_review_repo()
  logReady("script/analysis.R", reviewer = "alice")
  logRename("script/analysis.R", "script/renamed.R")
  expect_equal(nrow(reviewHistory("script/analysis.R")), 0)
  logComments("script/renamed.R")
  expect_equal(nrow(reviewHistory("script/renamed.R")), 2)
  expect_identical(reviewPending("bob")$file, "script/renamed.R")
})


test_that("each handoff records the acting user while preserving both roles", {
  local_review_repo()
  user <- "bob"
  local_mocked_bindings(review_user = function() user, .package = "review")
  file <- "script/analysis.R"
  logReady(file, reviewer = "alice")
  user <- "alice"
  logComments(file)
  user <- "bob"
  logAddressed(file)
  user <- "alice"
  logResolved(file)
  history <- reviewHistory(file)
  expect_identical(history$sender, c("bob", "alice", "bob", "alice"))
  expect_identical(history$recipient, c("alice", "bob", "alice", "bob"))
  expect_true(all(history$author == "bob"))
  expect_true(all(history$reviewer == "alice"))
})

test_that("handoffs record committed revisions and preserve existing origins", {
  local_review_repo()
  file <- "script/analysis.R"
  logAssign(file, origin = "script/other.R", reviewer = "alice")
  before <- logRead()
  logReady(file, reviewer = "alice")
  expect_identical(logRead(), before)
  writeLines("# revised analysis", file)
  svnRun("commit", file, "-m", "address feedback")
  logAddressed(file)
  expect_equal(reviewHistory(file)$revision, c(1, 2))
})
