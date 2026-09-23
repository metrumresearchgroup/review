#' Get the SVN status of a file or directory
#'
#' @param .file `character(1)` path to a file or directory tracked in SVN.
#' @param .relative_to Optional directory used to return paths relative to a
#'   working-copy root. By default, only basenames are returned.
#' @param .show_updates Include repository out-of-date information from
#'   `svn status -u`.
#'
#' @return `data.frame` with columns `path` (character) and
#'   `status` (character, e.g. `"modified"`, `"unversioned"`).
#'   Returns a zero-row data frame if the target is clean or if SVN fails.
#' @noRd
svnStatus <- function(.file, .relative_to = NULL, .show_updates = FALSE) {
  args <- if (.show_updates) c("-u", .file) else .file
  result <- tryCatch(
    svnXML("status", args),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(data.frame(path = character(), status = character()))
  }
  entries <- if (is.list(result$target)) result$target else list(result$target)
  paths <- purrr::map_chr(entries, function(e) {
    if (is.list(e) && !is.null(e$.attrs[["path"]])) {
      path <- e$.attrs[["path"]]
      if (is.null(.relative_to)) {
        basename(path)
      } else {
        fs::path_rel(path, .relative_to)
      }
    } else {
      NA_character_
    }
  })
  items <- purrr::map_chr(entries, function(e) {
    if (is.list(e) && !is.null(e[["wc-status"]])) {
      e[["wc-status"]][[".attrs"]][["item"]]
    } else {
      NA_character_
    }
  })
  keep <- !is.na(paths) & !is.na(items)
  out <- data.frame(
    path = paths[keep],
    status = items[keep],
    stringsAsFactors = FALSE
  )
  if (!.show_updates) return(out)

  repository_items <- purrr::map_chr(entries, function(e) {
    if (is.list(e) && !is.null(e[["repos-status"]])) {
      e[["repos-status"]][[".attrs"]][["item"]]
    } else {
      NA_character_
    }
  })
  out$remote_status <- repository_items[keep]
  out$out_of_date <- !is.na(out$remote_status) & out$remote_status != "none"
  out
}
