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
    .timeline { position: relative; padding-left: 28px; }
    .timeline:before { content:''; position:absolute; left:11px; top:8px; bottom:0; width:2px; background:#e2e8f0; }
    .rev { position:relative; margin:0 0 10px 0; cursor:pointer; }
    .rev:last-child { margin-bottom:0; }
    .rev:before { content:''; position:absolute; left:-23px; top:18px; width:14px; height:14px; border-radius:50%; background:#94a3b8; border:3px solid #f8fafc; box-shadow:0 0 0 2px #e2e8f0; transition:background-color .15s ease, box-shadow .15s ease, transform .15s ease; }
    .rev:hover:before { transform:scale(1.04); box-shadow:0 0 0 2px #cbd5e1; }
    .rev-card { border:1px solid #e2e8f0; border-radius:10px; background:#ffffff; padding:12px; box-shadow:0 1px 2px rgba(15,23,42,0.06); transition:border-color .15s ease, box-shadow .15s ease, background-color .15s ease; }
    .rev:hover .rev-card { box-shadow:0 4px 12px rgba(15,23,42,0.08); }
    .rev.sel:before { background:#4f46e5; box-shadow:0 0 0 3px #c7d2fe; }
    .rev.sel .rev-card { border-color:#818cf8; background:#f8faff; box-shadow:0 0 0 1px rgba(99,102,241,0.16), 0 6px 16px rgba(79,70,229,0.10); }
    .rev-main { display:flex; gap:10px; align-items:flex-start; min-width:0; }
    .rev-avatar { width:28px; height:28px; border-radius:999px; display:flex; align-items:center; justify-content:center; flex:0 0 auto; margin-top:1px; font-size:0.82rem; font-weight:700; color:#4338ca; background:linear-gradient(135deg, #e0e7ff 0%, #c7d2fe 100%); }
    .rev-body { flex:1 1 auto; min-width:0; }
    .rev-top { display:flex; align-items:flex-start; justify-content:space-between; gap:8px; }
    .rev-meta { display:flex; flex-wrap:wrap; gap:4px 6px; align-items:center; min-width:0; font-size:0.82rem; color:#64748b; line-height:1.3; }
    .rev-author { color:#0f172a; font-weight:600; }
    .rev-action, .rev-elapsed { white-space:nowrap; }
    .rev-tags { display:flex; gap:6px; align-items:center; flex-wrap:wrap; justify-content:flex-end; flex:0 0 auto; }
    .rev-id { display:inline-block; padding:0.2rem 0.45rem; border-radius:7px; border:1px solid #e2e8f0; background:#f8fafc; color:#475569; font-size:.72rem; font-weight:600; font-family:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, Courier New, monospace; line-height:1; }
    .rev.sel .rev-id { background:#eef2ff; border-color:#c7d2fe; color:#3730a3; }
    .rev-msg { margin-top:5px; color:#1e293b; font-size:0.92rem; font-weight:500; line-height:1.35; }
    .rev-msg em { color:#94a3b8; font-style:normal; font-weight:400; }
    .badge { display:inline-block; padding:0.15rem 0.45rem; border-radius:999px; font-size:.68rem; border:1px solid #e2e8f0; line-height:1.1; }
    .badge.y { background:#e7f6ec; color:#2f8f4e; border-color:#ccebd7; }
    .badge.n { background:#f4f4f4; color:#6c757d; }
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
    author <- trimws(as.character(row$author))
    avatar <- substr(author, 1L, 1L)
    if (!nzchar(avatar) || is.na(avatar)) {
      avatar <- "?"
    }
    message <- if (!is.null(row$msg) && nzchar(row$msg)) {
      row$msg
    } else {
      shiny::tags$em("(no message)")
    }
    shiny::div(
      class = cls,
      `data-rev` = id,
      shiny::div(
        class = "rev-card",
        shiny::div(
          class = "rev-main",
          shiny::div(class = "rev-avatar", toupper(avatar)),
          shiny::div(
            class = "rev-body",
            shiny::div(
              class = "rev-top",
              shiny::div(
                class = "rev-meta",
                shiny::span(class = "rev-author", author),
                shiny::span(class = "rev-action", "committed"),
                shiny::span(class = "rev-elapsed", row$elapsed)
              ),
              shiny::div(
                class = "rev-tags",
                qc,
                shiny::span(class = "rev-id", row$rev_display)
              )
            ),
            shiny::div(class = "rev-msg", message)
          )
        )
      )
    )
  })

  shiny::div(class = "timeline", rev_items)
}
