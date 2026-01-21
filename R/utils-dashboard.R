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

#' Build a dashboard title block with a label and relative path
#'
#' @param label `character(1)` label text shown before the path.
#' @param path `character(1)` path displayed as a relative path.
#'
#' @return `htmltools::tag` for use in a title slot.
#' @noRd
dashboard_title <- function(label, path) {
  htmltools::div(
    style = "display:flex; gap:.35rem; align-items:baseline; font-weight:600;",
    htmltools::span(label),
    htmltools::span(
      style = "opacity:.7; font-weight:400;",
      fs::path_rel(path)
    )
  )
}

#' Dashboard timeline CSS and behavior assets
#'
#' @param side_scroll_height `character(1)` CSS value for max-height.
#' @param extra_css `character(1)` optional additional CSS rules.
#'
#' @return `list` of HTML tags for use in a sidebar.
#' @noRd
timeline_assets <- function(side_scroll_height, extra_css = NULL) {
  base_css <- paste0(
    "
    .timeline { position: relative; padding-left: 20px; }
    .timeline:before { content:''; position:absolute; left:8px; top:0; bottom:0; width:2px; background:#e9ecef; }
    .rev { position:relative; margin:0 0 8px 0; padding:10px 10px 10px 14px; border:1px solid #e9ecef; border-radius:8px; background:#fff; cursor:pointer; }
    .rev:before { content:''; position:absolute; left:-6px; top:14px; width:12px; height:12px; border-radius:50%; background:#adb5bd; border:2px solid #fff; box-shadow:0 0 0 2px #e9ecef; }
    .rev:hover { box-shadow:0 2px 8px rgba(0,0,0,0.06); }
    .rev.sel { border-color:#0d6efd; background:#f5f9ff; }
    .rev.sel:before { background:#0d6efd; }
    .rev .h  { display:flex; gap:8px; align-items:center; flex-wrap:wrap; }
    .rev .id { font-weight:600; }
    .rev .m  { color:#6c757d; font-size:0.9rem; }
    .rev .msg{ margin-top:4px; color:#343a40; }
    .badge    { display:inline-block; padding:0.15rem 0.4rem; border-radius:999px; font-size:.72rem; border:1px solid #e9ecef; }
    .badge.y  { background:#e7f6ec; color:#2f8f4e; border-color:#ccebd7; }
    .badge.n  { background:#f4f4f4; color:#6c757d; }
    .badge.local { background:#eef3ff; color:#0d6efd; border-color:#d8e3ff; }
    .side-scroll { max-height: ", side_scroll_height, "; overflow:auto; padding-right:4px; }
    ",
    if (!is.null(extra_css)) extra_css else "",
    "
    "
  )

  list(
    shiny::tags$style(htmltools::HTML(base_css)),
    shiny::tags$script(htmltools::HTML(
      "
      document.addEventListener('click', function(e){
        var el = e.target.closest('.rev'); if(!el) return;
        var r = el.getAttribute('data-rev');
        if(window.Shiny && r) Shiny.setInputValue('rev_clicked', r, {priority:'event'});
      }, true);
    "
    ))
  )
}

#' Compute the default pair of selections from a revision log
#'
#' @param svn_log `data.frame` from `getRevHistory()`.
#'
#' @return `character()` with newest revision and "Local".
#' @noRd
default_selection_from_log <- function(svn_log) {
  revisions <- svn_log$rev
  newest <- max(as.numeric(revisions[revisions != "Local"]), na.rm = TRUE)
  c(as.character(newest), "Local")
}

#' Build a timeline UI for revision selection
#'
#' @param svn_log `data.frame` from `getRevHistory()`.
#' @param chosen `character()` currently selected revision IDs.
#'
#' @return `shiny::tag` timeline wrapper with revision items.
#' @noRd
render_timeline <- function(svn_log, chosen) {
  rev_items <- lapply(seq_len(nrow(svn_log)), function(i) {
    row <- svn_log[i, ]
    qc <- if (identical(row$QCed, "Yes")) {
      shiny::span(class = "badge y", "QCed")
    }
    id <- as.character(row$rev)
    cls <- if (id %in% chosen) "rev sel" else "rev"
    shiny::div(
      class = cls,
      `data-rev` = id,
      shiny::div(
        class = "h",
        shiny::span(class = "id", row$rev_display),
        shiny::span(class = "m", paste(row$author, ":", row$elapsed)),
        qc
      ),
      shiny::div(
        class = "msg",
        if (!is.null(row$msg) && nzchar(row$msg)) {
          row$msg
        } else {
          shiny::tags$em("(no message)")
        }
      )
    )
  })

  shiny::div(class = "timeline", rev_items)
}
