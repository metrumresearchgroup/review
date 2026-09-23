#' Add display metadata to parsed SVN status rows
#'
#' @param status A data frame returned by `svnStatus()`.
#' @noRd
decorateSvnStatus <- function(status) {
  symbols <- c(
    added = "A", conflicted = "C", deleted = "D", external = "X",
    ignored = "I", incomplete = "!", missing = "!", modified = "M",
    obstructed = "~", replaced = "R", unversioned = "?"
  )
  colors <- c(
    added = "#228833", conflicted = "#AA3377", deleted = "#EE6677",
    external = "#4477AA", ignored = "#BBBBBB", incomplete = "#CCBB44",
    missing = "#EE6677", modified = "#CCBB44", obstructed = "#EE6677",
    replaced = "#AA3377", unversioned = "#66CCEE"
  )
  text_colors <- c(
    added = "white", conflicted = "white", deleted = "#222222",
    external = "white", ignored = "#222222", incomplete = "#222222",
    missing = "#222222", modified = "#222222", obstructed = "#222222",
    replaced = "white", unversioned = "#222222"
  )

  status$symbol <- unname(symbols[status$status])
  status$symbol[is.na(status$symbol)] <- "-"
  status$color <- unname(colors[status$status])
  status$color[is.na(status$color)] <- "#BBBBBB"
  status$text_color <- unname(text_colors[status$status])
  status$text_color[is.na(status$text_color)] <- "#222222"
  if (!"out_of_date" %in% names(status)) {
    status$out_of_date <- rep(FALSE, nrow(status))
  }
  if (!"remote_status" %in% names(status)) {
    status$remote_status <- rep(NA_character_, nrow(status))
  }
  status$remote_symbol <- ifelse(status$out_of_date, "*", "-")
  status$remote_color <- ifelse(status$out_of_date, "#4477AA", "#BBBBBB")
  status$remote_text_color <- ifelse(status$out_of_date, "white", "#222222")
  status
}

#' Describe a command that may be previewed and executed by svnDashboard
#'
#' @noRd
dashboardCommand <- function(command, args, cwd, title, effect, risk, paths,
                             expected_status) {
  structure(
    list(
      command = command,
      args = as.character(args),
      cwd = cwd,
      title = title,
      effect = effect,
      risk = risk,
      paths = as.character(paths),
      expected_status = expected_status
    ),
    class = "svn_dashboard_command"
  )
}

#' Format a dashboard command as a copyable shell command
#'
#' @noRd
formatDashboardCommand <- function(command) {
  quote_arg <- function(x) {
    if (grepl("^[A-Za-z0-9_./:=+,-]+$", x)) x else shQuote(x, type = "sh")
  }
  paste(vapply(c(command$command, command$args), quote_arg, character(1)),
        collapse = " ")
}

#' Execute the exact command previously shown in an action preview
#'
#' @noRd
runDashboardCommand <- function(command) {
  tryCatch(
    processx::run(
      command = command$command,
      args = command$args,
      wd = command$cwd,
      echo = FALSE,
      error_on_status = FALSE
    ),
    error = function(e) {
      list(status = 1L, stdout = "", stderr = conditionMessage(e))
    }
  )
}

#' Check whether command targets still have their previewed SVN statuses
#'
#' @noRd
dashboardStatusIsCurrent <- function(command, current_status) {
  expected <- command$expected_status
  actual <- stats::setNames(current_status$status, current_status$path)
  all(vapply(
    names(expected),
    function(path) identical(unname(actual[path]), unname(expected[path])),
    logical(1)
  ))
}
