library(testthat)

createRepo <- function() {
  if (dir.exists("/tmp/svn-testing")) {
    try(system("rm -r /tmp/svn-testing/"))
    try(system("rm -rf /tmp/svn-testing/svn-proj-ABC-123"))
  }
  system("mkdir /tmp/svn-testing")
  system("svnadmin create /tmp/svn-testing/svn-proj-ABC-123")
  system("svn co file:///tmp/svn-testing/svn-proj-ABC-123 /tmp/svn-testing/test")
  return("/tmp/svn-testing/test")
}


