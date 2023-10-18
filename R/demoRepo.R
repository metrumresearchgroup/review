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
  curdir <- getwd()
  on.exit(setwd(curdir))
  repodir <- tempdir()

  # Create svn repo at specified locations
  system(glue::glue("svnadmin create {repodir}/svn-proj-{.project_name}"))
  system(glue::glue("svn co file://{repodir}/svn-proj-{.project_name} {repodir}/demo -q"))
  # Add scripts to the repo
  system(glue::glue("mkdir {repodir}/demo/script"))
  system(glue::glue("mkdir {repodir}/demo/script/pk"))
  
  
  setwd(glue::glue("{repodir}/demo"))
  
  create_file("script/data-assembly.R",
           paste("library(tidyverse)",
                 'src_abc <- mrgda::read_src_dir(here::here("data", "source", "STUDY-ABC"))',
                 "derived <- list(sl = list(),tv = list())",
                 'dm_0 <- src_abc$dm %>% filter(ACTARM != "Screen Failure")',
                 "derived$sl$dm <- dm_0",
                 sep = "\n"))
  create_file("script/combine-da.R",
           paste("library(tidyverse)",
                 "studies <- list()",
                 'studies$pk_abc <- readr::read_rds(here::here("data", "derived", "studies", "pk-abc.rds"))',
                 "pk_0 <- bind_rows(studies) %>% arrange(USUBJID, DATETIME)",
                 'pk_1 <- pk_0 %>% mrgda::assign_id(., "USUBJID")',
                 "pk_out <- pk_1",
                 sep = "\n"))
  create_file("script/pk/load-spec.R", 
           'pk_spec <- yspec::load_spec(here::here("script", "script/examp-yaml.yaml"))')
  create_file("script/examp-txt.txt", "This is the first version of the txt file")
  create_file("script/examp-yaml.yaml", "This is the first version of the yaml file")
  
  # Create QC log
  logCreate()
  
  # Check everything into SVN
  add_commit("first")
  
  # Assign and accept scripts in QC log
  logAccept("script/data-assembly.R")
  logAccept("script/pk/load-spec.R")
  logAccept("script/combine-da.R")
  logAssign("script/examp-txt.txt")
  
  # Check in updates to QC log
  add_commit("second")
  
  # Make edits to QCed file
  create_file("script/pk/load-spec.R", 
              'pk_spec <- yspec::load_spec(here::here("script", "examp-yaml.yaml"))')
  add_commit("third")
  
  create_file("script/data-assembly.R",
              paste("library(tidyverse)",
                    'source(here::here("script", "data-assembly", "da-functions.R"))',
                    'src_abc <- mrgda::read_src_dir(here::here("data", "source", "STUDY-ABC"))',
                    "derived <- list(sl = list(),tv = list())",
                    'dm_0 <- src_abc$dm %>% filter(ACTARM != "Screen Failure")',
                    "derived$sl$dm <- dm_0",
                    'pk_0 <- src_abc$pc %>% filter(PCTEST == "TEST OF INTEREST")',
                    "derived$tv$pc <- pk_0",
                    'ex_1 <- src_abc$ex %>% filter(EXTRT == "DRUG OF INTEREST")',
                    "derived$tv$dosing <- ex_1",
                    sep = "\n"))
  add_commit("fourth")
  
  create_file("README.md",
              paste("The following tasks are suggested to gain familiarity with the review package:",
                    '- run `diffQCed()` on "script/pk/load-spec.R" and "script/data-assembly.R"',
                    '- run `renderQCSummary()` on the "script" directory',
                    '- use `logAssign()` to add "script/examp-txt.txt" to the QC log',
                    '- run `logPending()` to see what scripts are in need of QC',
                    '- use `logAccept()` to sign off on any scripts with pending QC',
                    sep = "\n"))
  
  return(glue::glue('Demo SVN repo at "{repodir}/demo"'))
}

