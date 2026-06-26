#' Get the SVN status of a file or directory
#'
#' @param .file `character(1)` path to a file or directory tracked in SVN.
#'
#' @return `data.frame` with columns `path` (character, basename) and
#'   `status` (character, e.g. `"modified"`, `"unversioned"`).
#'   Returns a zero-row data frame if the target is clean or if SVN fails.
#' @noRd
svnStatus <- function(.file) {
  result <- tryCatch(
    svnCommand(.file = .file, .command = "status"),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(data.frame(path = character(), status = character()))
  }
  entries <- if (is.list(result$target)) result$target else list(result$target)
  paths <- vapply(
    entries,
    function(e) {
      if (is.list(e) && !is.null(e$.attrs[["path"]])) {
        basename(e$.attrs[["path"]])
      } else {
        NA_character_
      }
    },
    character(1L)
  )
  items <- vapply(
    entries,
    function(e) {
      if (is.list(e) && !is.null(e[["wc-status"]])) {
        e[["wc-status"]][[".attrs"]][["item"]]
      } else {
        NA_character_
      }
    },
    character(1L)
  )
  keep <- !is.na(paths) & !is.na(items)
  data.frame(path = paths[keep], status = items[keep], stringsAsFactors = FALSE)
}
