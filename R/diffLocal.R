#' Visual diff of local edits against the checked-out SVN revision
#'
#' Compares a file's local contents against its SVN BASE revision, like
#' `svn diff`. The comparison uses the revision checked out locally, even if
#' a newer revision is available in the repository.
#'
#' @inheritParams diffPreviousRevisions
#'
#' @examples
#' with_demoRepo({
#'   cat("\n# Local edit\n", file = "script/data-assembly.R", append = TRUE)
#'   diffLocal("script/data-assembly.R")
#' })
#'
#' @export
diffLocal <- function(.file,
                      .side_by_side = TRUE,
                      .ignore_white_space = FALSE,
                      .display_entire_file = FALSE,
                      .open_file = FALSE) {
  diffPreviousRevisions(
    .file = .file,
    .previous_revision = "BASE",
    .current_revision = NULL,
    .side_by_side = .side_by_side,
    .ignore_white_space = .ignore_white_space,
    .display_entire_file = .display_entire_file,
    .open_file = .open_file
  )
}
