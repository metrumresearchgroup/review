#' Create demo repo
#' 
#' @description 
#' Create a SVN repo with files checked in and a QC log. The purpose of this 
#' demo repo is for the user to become familiar with using review functions. 
#' 
#' The files checked into the repo are at various stages in terms of their QC
#' needs. This function returns the path to the repo, so that the user can set
#' their working directory to it. 
#' 
#' This demo repo is created in the `tmp` folder, so a new one will need to be
#' generated whenever the user restarts R. 
#' 
#' @param .project_name Character. Project name used to create the SVN repo
#'
#' @export
demoRepo <- function(.project_name) {
  
  curDir <- getwd()
  on.exit(setwd(curDir))
  
  repoInitPath <- fs::path_temp("svn-proj")
  
  if (fs::dir_exists(repoInitPath)) {
    fs::dir_delete(repoInitPath)
  }
  
  # Create svn repo at specified locations
  system(glue::glue("svnadmin create {repoInitPath}"))
  
  repoDir <- file.path(repoInitPath, .project_name)
  
  if (fs::dir_exists(repoDir)) {
    fs::dir_delete(repoDir)
  }
  
  system(glue::glue("svn co file://{repoInitPath} {repoDir} -q"))
  
  setwd(repoDir)
  
  # Add scripts to the repo
  fs::dir_create("script/pk", recurse = TRUE)
  fs::dir_create("deliv/figure")
  fs::dir_create("deliv/table")
  
  writeLines(
    c(
      "library(tidyverse)",
      'src_abc <- mrgda::read_src_dir(here::here("data", "source", "STUDY-ABC"))',
      "derived <- list(sl = list(),tv = list())",
      'dm_0 <- src_abc$dm %>% filter(ACTARM != "Screen Failure")',
      "derived$sl$dm <- dm_0"
    ),
    "script/data-assembly.R"
  )
  
  writeLines(
    c(
      "library(tidyverse)",
      "studies <- list()",
      'studies$pk_abc <- readr::read_rds(here::here("data", "derived", "studies", "pk-abc.rds"))',
      "pk_0 <- bind_rows(studies) %>% arrange(USUBJID, DATETIME)",
      'pk_1 <- pk_0 %>% mrgda::assign_id(., "USUBJID")',
      "pk_out <- pk_1"
    ),
    "script/combine-da.R"
  )
  
  writeLines(
    c(
      'pk_spec <- yspec::load_spec(here::here("script", "script/examp-yaml.yaml"))'
    ),
    "script/pk/load-spec.R"
  )
  
  writeLines(c('This is the first version of the txt file'),
             "script/examp-txt.txt")
  
  writeLines(c("This is the first version of the yaml file"),
             "script/examp-yaml.yaml")
  
  writeLines(c("This is the first version of the yml file"),
             "pkgr.yml")
  
  # Create QC log
  logCreate()
  
  # Check everything into SVN
  system("svn add * -q -q")
  system(glue::glue("svn commit -m 'initial commit' -q -q"))
  
  # Assign and accept scripts in QC log
  logAssign("script/data-assembly.R")
  logAssign("script/pk/load-spec.R")
  logAssign("script/combine-da.R")
  logAssign("script/examp-txt.txt")
  
  system(glue::glue("svn commit -m 'logAssign scripts ready for QC' -q -q"))
  
  logAccept("script/data-assembly.R")
  logAccept("script/pk/load-spec.R")
  logAccept("script/combine-da.R")
  
  # Check in updates to QC log
  system(glue::glue("svn commit -m 'logAccept scripts after QC' -q -q"))
  
  # Make edits to QCed file
  writeLines(
    c('pk_spec <- yspec::load_spec(here::here("script", "examp-yaml.yaml"))'),
    "script/pk/load-spec.R"
  )
  
  system(glue::glue("svn commit -m 'modify load-spec script' -q -q"))
  
  writeLines(
    c(
      "library(tidyverse)",
      'source(here::here("script", "data-assembly", "da-functions.R"))',
      'src_abc <- mrgda::read_src_dir(here::here("data", "source", "STUDY-ABC"))',
      "derived <- list(sl = list(),tv = list())",
      'dm_0 <- src_abc$dm %>% filter(ACTARM != "Screen Failure")',
      "derived$sl$dm <- dm_0",
      'pk_0 <- src_abc$pc %>% filter(PCTEST == "TEST OF INTEREST")',
      "derived$tv$pc <- pk_0",
      'ex_1 <- src_abc$ex %>% filter(EXTRT == "DRUG OF INTEREST")',
      "derived$tv$dosing <- ex_1"
    ),
    "script/data-assembly.R"
  )
  system(glue::glue("svn commit -m 'modify data-assembly' -q -q"))
  
  writeLines(
    c("The following tasks are suggested to gain familiarity with the review package:",
      '- run `diffQCed()` on "script/pk/load-spec.R" and "script/data-assembly.R"',
      '- run `renderQCSummary()`',
      '- use `logAssign()` to add "script/examp-txt.txt" to the QC log',
      '- run `logPending()` to see what scripts are in need of QC',
      '- use `logAccept()` to sign off on any scripts with pending QC'),
    "README.md"
  )
  
  grDevices::pdf("deliv/figure/example-pdf1.pdf", width = 8, height = 11)
  plot(1:10)
  grDevices::dev.off()
  
  grDevices::png("deliv/figure/example-png1.png")
  plot(1:10)
  grDevices::dev.off()
  
  grDevices::pdf("deliv/figure/example-pdf2.pdf", width = 11, height = 8)
  plot(5:10)
  plot(1:1000)
  grDevices::dev.off()
  
  grDevices::pdf("deliv/figure/example-pdf3.pdf")
  plot(1:10)
  grDevices::dev.off()
  
  pmdata <- pmtables::stdata()
  
  pmtables::stable_save(
    x = pmtables::stable(pmdata, panel = "STUDY"),
    file = "example-table-1.tex",
    dir = "deliv/table"
  )
  
  pmtables::stable_save(
    x = pmtables::stable_long(
      dplyr::bind_rows(
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata
      )
    ),
    file = "example-table-long-1.tex",
    dir = "deliv/table"
  )
  
  system("svn add 'deliv/figure/example-pdf1.pdf' -q -q")
  system("svn add 'deliv/figure/example-png1.png' -q -q")
  system("svn add 'deliv/figure/example-pdf2.pdf' -q -q")
  system("svn add 'deliv/figure/example-pdf3.pdf' -q -q")
  system("svn add 'deliv/table/example-table-1.tex' -q -q")
  system("svn add 'deliv/table/example-table-long-1.tex' -q -q")
  system(glue::glue("svn commit -m 'add pdf' -q -q"))
  
  Sys.sleep(1)
  
  grDevices::pdf("deliv/figure/example-pdf1.pdf", width = 8, height = 11)
  plot(5:1000)
  grDevices::dev.off()
  
  grDevices::png("deliv/figure/example-png1.png")
  plot(5:1000)
  grDevices::dev.off()
  
  Sys.sleep(1)
  
  grDevices::pdf("deliv/figure/example-pdf2.pdf", width = 11, height = 8)
  plot(1:2)
  plot(1:4)
  grDevices::dev.off()
  
  Sys.sleep(1)
  
  grDevices::pdf("deliv/figure/example-pdf3.pdf")
  plot(1:100)
  plot(1:3)
  grDevices::dev.off()
  
  grDevices::pdf("deliv/figure/example-pdf4.pdf")
  plot(1:300)
  grDevices::dev.off()
  
  # Make an edit to the table
  pmdata$N[1] <- "81"
  
  pmtables::stable_save(
    x = pmtables::stable(pmdata),
    file = "example-table-1.tex", 
    dir = "deliv/table"
  )
  
  pmtables::stable_save(
    x = pmtables::stable_long(
      dplyr::bind_rows(
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata,
        pmdata
      )
    ),
    file = "example-table-long-1.tex",
    dir = "deliv/table"
  )
  
  repoDir
}

#' @rdname demoRepo
#' 
#' @param code Executable code to run 
#' @export
with_demoRepo <- function(code, .project_name = "abc-123") {
  repo <- demoRepo(.project_name)
  withr::with_dir(repo, code)
}