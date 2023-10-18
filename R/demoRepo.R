createRepo <- function(.project_name) {
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
  review:::logCreate()
  
  # Check everything into SVN
  add_commit("first")
  
  # Assign and accept scripts in QC log
  review::logAccept("script/data-assembly.R")
  review::logAccept("script/pk/load-spec.R")
  review::logAccept("script/combine-da.R")
  review::logAssign("script/examp-txt.txt")
  
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
  
  # Can now show diffQCed with script/pk/load-spec.R and script/data-assembly.R
  # Can now run renderQCSummary
  # Can now logAssign script/examp-txt.txt
  
  return("/tmp/svn-demo/demo")
}


add_commit <- function(.name) {
  if (.name == "first") {
    system("svn add * -q -q")
  }
  system(glue::glue("svn commit -m '{.name} commit' -q -q")) 
}

create_file <- function(.name, .content) {
  system(glue::glue("echo '{.content}' > {.name}"))
}
