proj_name <- "abc-123"
repo <- demoRepo(proj_name)

test_that("demoRepo creates a temp directory with all expected files", {
  expect_true(dir.exists(repo))
  expect_equal(basename(repo), proj_name)
  expect_true(dir.exists(file.path(repo, "script/pk")))
  expect_true(file.exists(file.path(repo, "script", "data-assembly.R")))
  expect_true(file.exists(file.path(repo, "QClog.csv")))
})

test_that("demoRepo includes added and missing SVN dashboard examples", {
  status <- svnStatus(repo, .relative_to = repo)
  expect_identical(
    status$status[status$path == "script/added-example.R"],
    "added"
  )
  expect_identical(
    status$status[status$path == "script/deleted-example.R"],
    "missing"
  )
})

test_that("demoRepo includes a remotely updated dashboard example", {
  status <- svnStatus(repo, .relative_to = repo, .show_updates = TRUE)
  remote_only <- status[status$path == "script/remote-update-example.R", ]
  remote_and_local <- status[
    status$path == "script/remote-and-local-example.R",
  ]

  expect_equal(nrow(remote_only), 1L)
  expect_identical(remote_only$status, "normal")
  expect_true(remote_only$out_of_date)
  expect_identical(remote_only$remote_status, "modified")

  expect_equal(nrow(remote_and_local), 1L)
  expect_identical(remote_and_local$status, "modified")
  expect_true(remote_and_local$out_of_date)
  expect_identical(remote_and_local$remote_status, "modified")
})

qclog <- readr::read_csv(file.path(repo, "QClog.csv")) %>% suppressMessages()

test_that("demoRepo creates QC log and completes partial QC", {
  expect_true(
    qclog %>% 
      dplyr::count(revf == 0) %>% 
      dplyr::filter(`revf == 0`) %>% 
      dplyr::pull(n) == 4)
  
  expect_true(
    nrow(qclog %>% 
           dplyr::filter(file == "script/data-assembly.R")) == 2)
})

with_demoRepo({
  test_that("demoRepo sets up files for logPending to have example output", {
    expect_true(nrow(logPending()) == 3)
  })
})
