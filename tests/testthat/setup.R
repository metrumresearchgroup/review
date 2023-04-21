library(testthat)

createRepo <- function() {
  if (dir.exists("/tmp/svn-testing")) {
    try(system("rm -rf /tmp/svn-testing"))
  }
  system("mkdir /tmp/svn-testing")
  system("svnadmin create /tmp/svn-testing/svn-proj-ABC-123")
  system("svn co file:///tmp/svn-testing/svn-proj-ABC-123 /tmp/svn-testing/test -q")
  return("/tmp/svn-testing/test")
}

add_commit <- function(.name) {
  if (.name == "first") {
    system("svn add * -q -q")
  }
  system(glue::glue("svn commit -m '{.name} commit' -q -q")) 
}

add_file <- function(.name, .content) {
  system(glue::glue("echo '{.content}' > {.name}"))
}
