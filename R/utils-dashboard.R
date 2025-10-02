#' Update selected revision IDs based on a timeline click
#'
#' @description
#' Toggle a clicked revision in the current selection, enforcing a maximum
#' number of selections (default 2). Duplicates are removed while preserving
#' first-occurrence order; when adding beyond the limit, the most recent
#' selections are retained.
#'
#' @param ids `character()`
#'   Current selected revision identifiers (e.g., `"Local"`, `"105"`).
#' @param clicked `character(1)`
#'   The revision identifier that was clicked.
#' @param max_sel `integer(1)`
#'   Maximum number of selections to retain. Defaults to `2L`.
#'
#' @return `character()` updated selection.
#' @noRd
update_selection <- function(ids, clicked, max_sel = 2L) {
  clicked <- as.character(clicked)
  ids <- as.character(ids)
  
  if (length(clicked) != 1L || is.na(clicked) || !nzchar(clicked)) {
    return(ids)
  }
  
  if (clicked %in% ids) {
    new <- setdiff(ids, clicked)
  } else {
    new <- c(ids, clicked)
    new <- new[!duplicated(new)]
    if (length(new) > max_sel) new <- utils::tail(new, max_sel)
  }
  
  new
}

#' Compute a paired selection (prior/newer) from selected revision IDs
#'
#' @description
#' Given a character vector of selected revision identifiers (e.g., `"Local"`
#' and/or numeric revisions as strings), compute the revision pair used for
#' diffing. If `"Local"` is present, the local working copy is treated as the
#' **newer** side and the minimum numeric revision among the other selections is
#' treated as **prior**. If only numeric revisions are present, the two smallest
#' numeric revisions are used as `prior` and `newer` respectively.
#'
#' @param ids `character()`
#'   Selected revision identifiers. May include `"Local"` and/or numeric
#'   revisions represented as character strings. Duplicates are ignored.
#'
#' @return `list` with elements:
#' \describe{
#'   \item{ids}{The de-duplicated input `ids` (character).}
#'   \item{prior}{`numeric(1)` the prior revision number, or `NULL` if a pair
#'     cannot be determined.}
#'   \item{newer}{`numeric(1)` the newer revision number, or `NULL` to indicate
#'     the local working copy when `"Local"` is selected.}
#' }
#'
#' @details
#' If fewer than two valid selections are provided, both `prior` and `newer`
#' are `NULL`. Non-numeric entries other than `"Local"` are ignored when
#' computing numeric revisions.
#'
#' @examples
#' compute_selection(c("105", "Local"))
#' compute_selection(c("101", "103"))
#' compute_selection("Local")
#'
#' @noRd
compute_selection <- function(ids) {
  ids <- ids[!duplicated(ids)]
  if (length(ids) < 2) {
    return(list(ids = ids, prior = NULL, newer = NULL))
  }
  
  if ("Local" %in% ids) {
    other <- setdiff(ids, "Local")
    other_num <- suppressWarnings(as.numeric(other))
    other_num <- other_num[!is.na(other_num)]
    if (!length(other_num)) {
      return(list(ids = ids, prior = NULL, newer = NULL))
    }
    return(list(ids = ids, prior = min(other_num), newer = NULL))
  } else {
    nums <- suppressWarnings(as.numeric(ids))
    nums <- sort(nums)
    if (length(nums) < 2) {
      return(list(ids = ids, prior = NULL, newer = NULL))
    }
    return(list(ids = ids, prior = nums[1], newer = nums[2]))
  }
}

