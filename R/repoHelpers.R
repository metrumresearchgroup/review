#' @keywords internal
add_commit <- function(.name) {
  if (.name == "first") {
    system("svn add * -q -q")
  }
  system(glue::glue("svn commit -m '{.name} commit' -q -q")) 
}
#' @keywords internal
create_file <- function(.name, .content) {
  system(glue::glue("echo '{.content}' > {.name}"))
}