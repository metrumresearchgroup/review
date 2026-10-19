#' Record a review handoff
#'
#' These functions record repeatable handoffs in `Reviewlog.csv`, alongside
#' `QClog.csv`. They do not edit script comments or send notifications.
#'
#' @param file A single file path, relative to `directory` or absolute.
#' @param reviewer Username to tag as reviewer. Defaults to the established
#'   reviewer for this file, or an interactive username picker.
#' @param author Username to tag as author. Defaults to the established author
#'   for this file, or an interactive username picker.
#' @param comment Optional character string with extra handoff information.
#' @param directory Directory from which `file` is resolved. The QC log must
#'   exist here or in a parent directory.
#'
#' @details
#' `logReady()` requests initial review or final acceptance after cleanup. It
#' assigns files missing from `QClog.csv` using the selected reviewer.
#' `logComments()` hands reviewer feedback to the author. `logAddressed()`
#' hands the author's responses back to the reviewer. These two actions can
#' repeat. `logResolved()` tells the author that comments can be removed;
#' after cleanup, the author calls `logReady()` and the reviewer `logAccept()`.
#'
#' The first handoff identifies its sender using `Sys.info()[["user"]]`, as
#' `logAccept()` does. Subsequent handoffs reuse the recorded author and reviewer.
#' An explicit recipient updates that role for subsequent handoffs. The picker
#' offers usernames from the QC log and review history, excluding `"anyone"`.
#' In noninteractive sessions, an unknown recipient must be supplied explicitly.
#' Commit changes before recording a handoff: the revision records SVN's last
#' committed revision, rather than uncommitted script contents.
#'
#' @return Invisibly returns the recorded event as a tibble.
#' @examples
#' with_demoRepo({
#'   file <- "script/examp-yaml.yaml"
#'   logReady(file, reviewer = "alice", comment = "Ready for review.")
#'   logComments(file, comment = "Please check missing values.")
#'   logAddressed(file, comment = "Updated the handling.")
#'   logResolved(file, comment = "Please remove the comments.")
#'   logReady(file)
#'   logAccept(file)
#'   reviewHistory(file)
#' })
#' @importFrom rlang .data .env
#' @export
logReady <- function(file, reviewer = NULL, comment = NULL,
                     directory = getwd()) {
  review_handoff(file, "ready", reviewer, comment, directory)
}

#' @rdname logReady
#' @export
logComments <- function(file, author = NULL, comment = NULL,
                        directory = getwd()) {
  review_handoff(file, "comments", author, comment, directory)
}

#' @rdname logReady
#' @export
logAddressed <- function(file, reviewer = NULL, comment = NULL,
                         directory = getwd()) {
  review_handoff(file, "addressed", reviewer, comment, directory)
}

#' @rdname logReady
#' @export
logResolved <- function(file, author = NULL, comment = NULL,
                        directory = getwd()) {
  review_handoff(file, "resolved", author, comment, directory)
}

#' View review handoffs
#'
#' @param file Optional single file path, relative to `directory` or absolute.
#'   If omitted, returns the history for all files.
#' @param directory Directory at or below the QC log root.
#' @return A tibble of events in recording order, including file, action,
#'   sender, recipient, author, reviewer, revision, GMT time, and comment.
#' @export
reviewHistory <- function(file = NULL, directory = getwd()) {
  root <- review_root(directory)
  history <- review_read(root)
  if (!is.null(file)) {
    path <- review_path(file, directory, root)
    history <- dplyr::filter(history, .data$file == path)
  }
  history
}

#' View review handoffs awaiting a user
#'
#' @param user Username whose pending handoffs to return. Defaults to the
#'   current operating-system user, matching `logAccept()`.
#' @param directory Directory at or below the QC log root.
#' @return A tibble containing the latest event for each file currently handed
#'   to `user`. Acceptance closes a handoff. This is distinct from `logPending()`,
#'   which checks whether committed file and origin revisions need QC.
#' @export
reviewPending <- function(user = Sys.info()[["user"]], directory = getwd()) {
  review_string(user, "user")
  reviewHistory(directory = directory) %>%
    dplyr::group_by(.data$file) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$action != "accepted", .data$recipient == user)
}

review_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(stringr::str_trim(value))) {
    cli::cli_abort("'{name}' must be a single non-empty character string.")
  }
}

review_root <- function(directory) {
  root <- logRoot(directory)
  if (is.null(root)) {
    cli::cli_abort("QC log does not exist. Run logCreate() first.")
  }
  root
}

review_path <- function(file, directory, root) {
  review_string(file, "file")
  path <- as.character(fs::path_rel(fs::path_abs(file, start = directory), root))
  if (path == ".." || stringr::str_starts(path, stringr::fixed("../")) || fs::is_absolute_path(path)) {
    cli::cli_abort("'file' must be within the QC log root.")
  }
  path
}

review_read <- function(root) {
  empty <- dplyr::tibble(
    file = character(), action = character(), sender = character(),
    recipient = character(), author = character(), reviewer = character(),
    revision = double(), time = character(), comment = character()
  )
  path <- fs::path(root, "Reviewlog.csv")
  if (!fs::file_exists(path)) {
    return(empty)
  }
  history <- readr::read_csv(
    path, col_types = readr::cols(.default = readr::col_character()),
    na = "", name_repair = "minimal", show_col_types = FALSE, progress = FALSE
  )
  if (!identical(names(history), names(empty))) {
    cli::cli_abort("Reviewlog.csv has unexpected columns.")
  }
  history$revision <- as.numeric(history$revision)
  history
}

review_write <- function(history, root) {
  readr::write_csv(history, fs::path(root, "Reviewlog.csv"), na = "")
}

review_choose_user <- function(users, role) {
  if (!interactive()) {
    cli::cli_abort("Supply '{role}' explicitly in a noninteractive session.")
  }
  choices <- c(users, "Enter a new username")
  selected <- utils::menu(choices, title = paste("Select", role))
  if (selected == 0L) {
    cli::cli_abort("Review handoff cancelled.")
  }
  if (selected == length(choices)) {
    return(readline(paste0(role, " username: ")))
  }
  choices[[selected]]
}

review_handoff <- function(file, action, recipient, comment, directory) {
  root <- review_root(directory)
  path <- review_path(file, directory, root)
  full_path <- fs::path(root, path)
  if (!fs::file_exists(full_path)) {
    cli::cli_abort("File does not exist: {.path {file}}")
  }
  if (!is.null(comment)) {
    review_string(comment, "comment")
  }

  # Existing log helpers resolve stored paths from the current log root.
  previous_dir <- getwd()
  on.exit(setwd(previous_dir))
  setwd(root)
  qc <- logRead(root)
  history <- review_read(root)
  previous <- history %>%
    dplyr::filter(.data$file == path) %>%
    dplyr::slice_tail(n = 1)
  sender <- review_user()
  author <- sender
  reviewer <- sender
  if (nrow(previous) > 0L) {
    author <- previous$author[[1]]
    reviewer <- previous$reviewer[[1]]
  }
  role <- if (action %in% c("ready", "addressed")) "reviewer" else "author"
  if (is.null(recipient) && nrow(previous) > 0L) {
    recipient <- previous[[role]][[1]]
  }
  if (is.null(recipient)) {
    users <- dplyr::tibble(user = c(qc$reviewer, history$author, history$reviewer)) %>%
      dplyr::filter(!is.na(.data$user), nzchar(.data$user), .data$user != "anyone") %>%
      dplyr::distinct(.data$user) %>%
      dplyr::arrange(.data$user) %>%
      dplyr::pull(.data$user)
    recipient <- review_choose_user(users, role)
  }
  review_string(recipient, role)
  if (recipient == "anyone") {
    cli::cli_abort("Select a named {role} rather than 'anyone'.")
  }
  if (role == "reviewer") reviewer <- recipient else author <- recipient
  rev <- revision(full_path)
  if (is.na(rev)) {
    cli::cli_abort("File must be tracked in SVN before recording a handoff.")
  }
  event <- dplyr::tibble(
    file = path, action = action, sender = sender, recipient = recipient,
    author = author, reviewer = reviewer, revision = rev,
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6 GMT", tz = "GMT"),
    comment = if (is.null(comment)) NA_character_ else comment
  )
  if (action == "ready" && !path %in% qc$file) {
    logAssign(path, directory = root, reviewer = reviewer)
  }
  review_write(dplyr::bind_rows(history, event), root)
  cli::cli_alert_info("{path}: {action}; tagged {recipient}.")
  invisible(event)
}

# Acceptance is recorded only for files already using review handoffs.
review_accept <- function(accepted, root) {
  history <- review_read(root)
  events <- purrr::pmap(accepted, function(file, revf, reviewer, ...) {
    previous <- history %>%
      dplyr::filter(.data$file == .env$file) %>%
      dplyr::slice_tail(n = 1)
    if (nrow(previous) == 0L) return(NULL)
    previous %>%
      dplyr::mutate(
        action = "accepted", sender = .env$reviewer, recipient = .data$author,
        reviewer = .env$reviewer, revision = .env$revf,
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6 GMT", tz = "GMT"),
        comment = NA_character_
      )
  }) %>% dplyr::bind_rows()
  if (nrow(events) > 0L) {
    review_write(dplyr::bind_rows(history, events), root)
  }
  invisible(NULL)
}

review_user <- function() {
  Sys.info()[["user"]]
}
