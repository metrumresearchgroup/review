#' Launch an interactive SVN working-copy dashboard
#'
#' @description
#' `svnDashboard()` displays local SVN changes, previews file differences, and
#' provides explicitly confirmed SVN and filesystem actions. Every action shows
#' the complete command, working directory, and affected paths before it runs.
#'
#' @param .path `character(1)` Path to the top of an SVN working copy. Defaults
#'   to the project root returned by `here::here()`.
#'
#' @examples
#' \dontrun{
#' svnDashboard()
#' svnDashboard("path/to/working-copy")
#' }
#'
#' @export
svnDashboard <- function(.path = here::here()) {
  if (!fs::is_dir(.path)) {
    stop("`.path` must be an SVN working-copy directory.")
  }

  repo_path <- fs::path_abs(.path)
  initial_status <- decorateSvnStatus(
    svnStatus(repo_path, .relative_to = repo_path, .show_updates = TRUE)
  )
  revision_dir <- file.path(tempdir(), "review-svn-dashboard")
  dir.create(revision_dir, showWarnings = FALSE, recursive = TRUE)

  shiny::addResourcePath("svn-local", repo_path)
  shiny::addResourcePath("svn-revisions", revision_dir)

  ui <- bslib::page_sidebar(
    title = shiny::div(
      class = "svn-title",
      dashboard_title("SVN Status:", basename(repo_path)),
      shiny::uiOutput("status_counts", inline = TRUE)
    ),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    sidebar = bslib::sidebar(
      open = "always",
      width = 410,
      shiny::tags$style(htmltools::HTML(
        "
        .svn-status-row { display:flex; align-items:center; gap:.55rem; padding:.55rem .65rem; }
        .svn-status-row.active { background:#eef2ff; box-shadow:inset 3px 0 #4477AA; }
        .svn-file { flex:1 1 auto; min-width:0; border:0; padding:0; background:transparent; text-align:left; }
        .svn-file:hover { text-decoration:underline; }
        .svn-path { overflow-wrap:anywhere; }
        .command-output { max-height:260px; overflow:auto; white-space:pre-wrap; margin:0; }
        .btn.svn-action-add { background:#dcfce7; border-color:#86efac; color:#166534; }
        .btn.svn-action-delete { background:#fee2e2; border-color:#fca5a5; color:#991b1b; }
        .btn.svn-action-commit { background:#4477AA; border-color:#4477AA; color:white; }
        .btn.svn-action-clear { background:#E5E7EB; border-color:#d1d5db; color:#343a40; }
        .alert.svn-preview-add { background:#dcfce7; border-color:#86efac; color:#166534; }
        .svn-title { display:flex; align-items:center; gap:.75rem; flex-wrap:wrap; }
        .svn-counts { display:flex; gap:.3rem; align-items:center; }
        .btn.svn-action-add:hover, .btn.svn-action-delete:hover,
        .btn.svn-action-commit:hover { filter:brightness(.96); }
        "
      )),
      shiny::tags$script(htmltools::HTML(
        "
        document.addEventListener('click', function(e) {
          var el = e.target.closest('.svn-file');
          if (!el) return;
          e.preventDefault();
          document.querySelectorAll('.svn-status-row.active').forEach(function(x) {
            x.classList.remove('active');
          });
          el.closest('.svn-status-row').classList.add('active');
          if (window.Shiny) {
            Shiny.setInputValue('status_clicked', el.dataset.path, {priority:'event'});
          }
        });
        document.addEventListener('change', function(e) {
          if (!e.target.matches('.commit-path')) return;
          var selected = Array.from(document.querySelectorAll('.commit-path:checked'))
            .map(function(x) { return x.dataset.path; });
          if (window.Shiny) {
            Shiny.setInputValue('status_selection', selected, {priority:'event'});
            Shiny.setInputValue('manual_selection_changed', Date.now(), {priority:'event'});
          }
        });
        if (window.Shiny) {
          Shiny.addCustomMessageHandler('setStatusSelection', function(paths) {
            document.querySelectorAll('.commit-path').forEach(function(x) {
              x.checked = paths.indexOf(x.dataset.path) !== -1;
            });
            Shiny.setInputValue('status_selection', paths, {priority:'event'});
          });
        }
        "
      )),
      shiny::div(
        class = "d-grid gap-2 mb-3",
        shiny::uiOutput("selection_controls"),
        shiny::uiOutput("selection_actions")
      ),
      shiny::div(
        class = "text-muted small mb-2",
        "Local status | Repository update (* = newer)"
      ),
      shiny::uiOutput("status_list")
    ),
    shiny::div(
      class = "p-2",
      shiny::uiOutput("file_actions"),
      shiny::uiOutput("file_view")
    )
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() shiny::stopApp())
    show_app_exit_hint("svnDashboard")

    read_status <- function() {
      decorateSvnStatus(svnStatus(
        repo_path,
        .relative_to = repo_path,
        .show_updates = TRUE
      ))
    }
    status_state <- shiny::reactiveVal(initial_status)
    selected_path <- shiny::reactiveVal(NULL)
    pending_command <- shiny::reactiveVal(NULL)
    commit_targets <- shiny::reactiveVal(NULL)

    refresh <- function(clear_selection = TRUE) {
      status_state(read_status())
      if (clear_selection) {
        session$sendCustomMessage("setStatusSelection", character())
        shiny::updateSelectInput(session, "select_all_status", selected = "")
      }
    }

    command_for <- function(command, args, title, effect, risk, paths,
                            status = status_state()) {
      expected <- stats::setNames(
        status$status[match(paths, status$path)],
        paths
      )
      dashboardCommand(
        command = command,
        args = args,
        cwd = repo_path,
        title = title,
        effect = effect,
        risk = risk,
        paths = paths,
        expected_status = expected
      )
    }

    preview_command <- function(command) {
      pending_command(command)
      risk_class <- switch(
        command$risk,
        add = "svn-preview-add",
        destructive = "alert-danger",
        repository = "alert-warning",
        caution = "alert-warning",
        "alert-info"
      )
      confirm_class <- if (command$risk == "destructive") {
        "btn-danger"
      } else if (command$risk == "add") {
        "svn-action-add"
      } else {
        "btn-primary"
      }
      shiny::showModal(shiny::modalDialog(
        title = command$title,
        shiny::div(class = paste("alert", risk_class), command$effect),
        shiny::tags$strong("Working directory"),
        shiny::tags$pre(repo_path),
        shiny::tags$strong("Exact command"),
        shiny::tags$pre(paste0("$ ", formatDashboardCommand(command))),
        shiny::tags$strong("Affected paths"),
        shiny::div(
          style = "max-height:220px; overflow:auto;",
          shiny::tags$ul(lapply(command$paths, shiny::tags$li))
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "confirm_action", "Run this command", class = confirm_class
          )
        ),
        easyClose = FALSE,
        size = "l"
      ))
    }

    selected_item <- shiny::reactive({
      path <- selected_path()
      shiny::req(path)
      status <- status_state()
      index <- match(path, status$path)
      shiny::req(!is.na(index))
      status[index, , drop = FALSE]
    })

    shiny::observeEvent(input$status_clicked, {
      selected_path(as.character(input$status_clicked))
    })

    output$status_list <- shiny::renderUI({
      status <- status_state()
      if (!nrow(status)) {
        return(shiny::div(class = "alert alert-success", "Working copy is clean."))
      }

      items <- lapply(seq_len(nrow(status)), function(i) {
        row <- status[i, ]
        shiny::div(
          class = "svn-status-row list-group-item",
          shiny::tags$input(
            type = "checkbox", class = "form-check-input commit-path",
            `data-path` = row$path, title = "Select for action"
          ),
          shiny::tags$button(
            type = "button", class = "svn-file", `data-path` = row$path,
            shiny::span(row$path, class = "svn-path")
          ),
          shiny::span(
            row$symbol,
            title = row$status,
            class = "badge",
            style = paste0(
              "background:", row$color, ";color:", row$text_color, ";"
            )
          ),
          shiny::span(
            row$remote_symbol,
            title = if (row$out_of_date) {
              paste0("Newer repository version available: ", row$remote_status)
            } else {
              "No newer repository version"
            },
            class = "badge",
            style = paste0(
              "background:", row$remote_color,
              ";color:", row$remote_text_color, ";"
            )
          )
        )
      })
      shiny::div(class = "list-group", items)
    })

    output$status_counts <- shiny::renderUI({
      status <- status_state()
      symbols <- status$symbol[status$symbol != "-"]
      counts <- table(symbols)
      symbol_order <- c("M", "A", "D", "!", "?", "R", "C", "~", "X", "I")
      present <- symbol_order[symbol_order %in% names(counts)]

      badges <- lapply(present, function(symbol) {
        index <- match(symbol, status$symbol)
        shiny::span(
          paste(symbol, unname(counts[[symbol]])),
          class = "badge",
          style = paste0(
            "background:", status$color[[index]],
            ";color:", status$text_color[[index]], ";"
          )
        )
      })
      remote_count <- sum(status$out_of_date)
      if (remote_count) {
        badges <- c(badges, list(shiny::span(
          paste("*", remote_count),
          class = "badge",
          style = "background:#4477AA;color:white;"
        )))
      }
      shiny::div(class = "svn-counts", badges)
    })

    selected_paths <- shiny::reactive({
      paths <- as.character(unlist(input$status_selection, use.names = FALSE))
      status <- status_state()
      unique(paths[paths %in% status$path])
    })

    output$selection_controls <- shiny::renderUI({
      status <- status_state()
      symbols <- unique(status$symbol[status$symbol != "-"])
      symbol_order <- c("M", "A", "D", "!", "?", "R", "C", "~", "X", "I")
      symbols <- symbol_order[symbol_order %in% symbols]
      status_labels <- c(
        M = "Modified (M)", A = "Added to SVN (A)",
        D = "Deleted in SVN (D)", `!` = "Missing locally (!)",
        `?` = "Untracked (?)", R = "Replaced (R)",
        C = "Conflicted (C)", `~` = "Obstructed (~)",
        X = "External (X)", I = "Ignored (I)"
      )
      choices <- stats::setNames(symbols, unname(status_labels[symbols]))
      if (any(status$out_of_date)) {
        choices <- c(choices, "Repository updates (*)" = "__remote__")
      }

      shiny::tagList(
        shiny::selectInput(
          "select_all_status",
          label = NULL,
          choices = c(
            "Select files..." = "",
            "All listed files" = "__all__",
            choices
          ),
          selected = ""
        ),
        shiny::actionButton(
          "clear_selection", "Clear selection",
          class = "svn-action-clear w-100"
        )
      )
    })

    shiny::observeEvent(input$select_all_status, {
      symbol <- input$select_all_status
      if (is.null(symbol) || !nzchar(symbol)) return()

      status <- status_state()
      paths <- if (symbol == "__all__") {
        status$path
      } else if (symbol == "__remote__") {
        status$path[status$out_of_date]
      } else {
        status$path[status$symbol == symbol]
      }
      session$sendCustomMessage("setStatusSelection", paths)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_selection, {
      session$sendCustomMessage("setStatusSelection", character())
      shiny::updateSelectInput(session, "select_all_status", selected = "")
    })

    shiny::observeEvent(input$manual_selection_changed, {
      shiny::updateSelectInput(session, "select_all_status", selected = "")
    }, ignoreInit = TRUE)

    output$selection_actions <- shiny::renderUI({
      status <- status_state()
      paths <- selected_paths()
      selected <- status[match(paths, status$path), , drop = FALSE]
      if (!nrow(selected)) return(NULL)

      untracked <- selected$path[selected$status == "unversioned"]
      untracked <- untracked[!fs::is_dir(fs::path(repo_path, untracked))]
      deletable <- selected$path[
        selected$status %in% c("modified", "replaced", "missing")
      ]
      committable <- selected$path[
        selected$status %in% c("added", "deleted", "modified", "replaced")
      ]

      buttons <- list()
      if (length(untracked)) {
        buttons <- c(buttons, list(
          shiny::actionButton(
            "add_selected", "Add selected", class = "svn-action-add w-100"
          ),
          shiny::actionButton(
            "delete_selected_local", "Delete selected locally",
            class = "svn-action-delete w-100 mt-2"
          )
        ))
      }
      if (length(deletable)) {
        buttons <- c(buttons, list(shiny::actionButton(
          "svn_delete_selected", "Schedule SVN deletion",
          class = "svn-action-delete w-100 mt-2"
        )))
      }
      if (length(committable)) {
        buttons <- c(buttons, list(shiny::actionButton(
          "commit_selected", "Commit selected...",
          class = "svn-action-commit w-100 mt-2"
        )))
      }
      shiny::tagList(buttons)
    })

    output$file_actions <- shiny::renderUI({
      item <- selected_item()
      path <- item$path[[1]]
      status <- item$status[[1]]
      is_file <- file.exists(fs::path(repo_path, path)) &&
        !fs::is_dir(fs::path(repo_path, path))

      buttons <- list()
      if (status == "unversioned" && is_file) {
        buttons <- c(buttons, list(
          shiny::actionButton("file_add", "Add to SVN", class = "btn-success"),
          shiny::actionButton(
            "file_delete_local", "Delete local file", class = "btn-outline-danger"
          )
        ))
      }
      if (status %in% c("modified", "replaced", "missing")) {
        buttons <- c(buttons, list(
          shiny::actionButton(
            "file_svn_delete", "Schedule SVN deletion",
            class = "btn-outline-danger"
          )
        ))
      }
      if (!length(buttons)) return(NULL)
      shiny::div(class = "d-flex gap-2 mb-3", buttons)
    })

    shiny::observeEvent(input$file_add, {
      item <- selected_item()
      path <- item$path[[1]]
      preview_command(command_for(
        "svn", c("add", "--", path), "Add file to SVN",
        "Schedules this unversioned file for addition. The repository is not changed until commit.",
        "add", path
      ))
    })

    shiny::observeEvent(input$file_delete_local, {
      item <- selected_item()
      path <- item$path[[1]]
      preview_command(command_for(
        "rm", c("--", path), "Delete untracked local file",
        "Permanently deletes this unversioned local file. SVN cannot restore it.",
        "destructive", path
      ))
    })

    shiny::observeEvent(input$file_svn_delete, {
      item <- selected_item()
      path <- item$path[[1]]
      is_missing <- item$status[[1]] == "missing"
      preview_command(command_for(
        "svn", c("delete", "--force", "--", path), "Schedule SVN deletion",
        if (is_missing) {
          "Schedules this already-missing file for repository deletion in the next commit."
        } else {
          "Deletes the local file, including any uncommitted edits, and schedules its repository deletion for the next commit."
        },
        if (is_missing) "caution" else "destructive", path
      ))
    })

    shiny::observeEvent(input$add_selected, {
      status <- status_state()
      paths <- selected_paths()
      paths <- paths[paths %in% status$path[status$status == "unversioned"]]
      paths <- paths[!fs::is_dir(fs::path(repo_path, paths))]
      shiny::req(length(paths))
      preview_command(command_for(
        "svn", c("add", "--", paths), "Add selected files",
        paste0("Schedules ", length(paths), " selected unversioned files for addition."),
        "add", paths
      ))
    })

    shiny::observeEvent(input$delete_selected_local, {
      status <- status_state()
      paths <- selected_paths()
      paths <- paths[paths %in% status$path[status$status == "unversioned"]]
      paths <- paths[!fs::is_dir(fs::path(repo_path, paths))]
      shiny::req(length(paths))
      preview_command(command_for(
        "rm", c("--", paths), "Delete selected local files",
        paste0("Permanently deletes ", length(paths), " selected unversioned local files. SVN cannot restore these files."),
        "destructive", paths
      ))
    })

    shiny::observeEvent(input$svn_delete_selected, {
      status <- status_state()
      paths <- selected_paths()
      eligible <- status$status %in% c("modified", "replaced", "missing")
      paths <- paths[paths %in% status$path[eligible]]
      shiny::req(length(paths))
      selected_status <- status$status[match(paths, status$path)]
      has_local_changes <- any(selected_status != "missing")
      preview_command(command_for(
        "svn", c("delete", "--force", "--", paths),
        "Schedule selected SVN deletions",
        if (has_local_changes) {
          paste0("Deletes ", length(paths), " selected local files, including any uncommitted edits, and schedules their repository deletion for the next commit.")
        } else {
          paste0("Schedules ", length(paths), " already-missing files for repository deletion in the next commit.")
        },
        if (has_local_changes) "destructive" else "caution", paths
      ))
    })

    shiny::observeEvent(input$commit_selected, {
      paths <- selected_paths()
      status <- status_state()
      allowed <- c("added", "deleted", "modified", "replaced")
      paths <- paths[paths %in% status$path[status$status %in% allowed]]
      if (!length(paths)) {
        shiny::showNotification("Select at least one committable path.", type = "error")
        return()
      }
      commit_targets(list(
        paths = paths,
        status = status
      ))
      shiny::showModal(shiny::modalDialog(
        title = "Commit selected paths",
        shiny::textAreaInput(
          "commit_message", "Commit message", rows = 4,
          placeholder = "Describe the changes being committed"
        ),
        shiny::div(
          class = "text-muted",
          paste(length(paths), "selected path(s). The full command will be shown before it runs.")
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("preview_commit", "Preview command", class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    })

    shiny::observeEvent(input$preview_commit, {
      message <- if (is.null(input$commit_message)) "" else input$commit_message
      message <- trimws(message)
      if (!nzchar(message)) {
        shiny::showNotification("A commit message is required.", type = "error")
        return()
      }
      targets <- commit_targets()
      shiny::req(targets)
      command <- command_for(
        "svn", c("commit", "-m", message, "--", targets$paths),
        "Commit selected paths",
        paste0("Commits ", length(targets$paths), " selected working-copy changes to the SVN repository."),
        "repository", targets$paths, status = targets$status
      )
      shiny::removeModal()
      preview_command(command)
    })

    shiny::observeEvent(input$confirm_action, {
      command <- pending_command()
      shiny::req(command)

      current <- read_status()
      if (!dashboardStatusIsCurrent(command, current)) {
        note <- "Not run: SVN status changed after the command was previewed."
        status_state(current)
        shiny::removeModal()
        shiny::showNotification(note, type = "error", duration = NULL)
        return()
      }

      result <- runDashboardCommand(command)
      success <- identical(as.integer(result$status), 0L)
      pending_command(NULL)
      shiny::removeModal()
      refresh()

      if (success) {
        shiny::showNotification("Command completed successfully.", type = "message")
      } else {
        shiny::showNotification(
          paste0("Command failed with exit status ", result$status, "."),
          type = "error", duration = NULL
        )
      }
    })

    output$file_view <- shiny::renderUI({
      item <- selected_item()
      relative_path <- item$path[[1]]
      file_status <- item$status[[1]]
      file_path <- fs::path(repo_path, relative_path)
      is_new <- file_status %in% c("unversioned", "added")
      extension <- tolower(tools::file_ext(file_path))
      is_figure <- extension %in% c("png", "pdf")

      heading <- shiny::div(
        class = "d-flex align-items-center gap-2 mb-3",
        shiny::tags$h4(relative_path, class = "m-0"),
        shiny::span(
          item$symbol[[1]], title = file_status, class = "badge",
          style = paste0(
            "background:", item$color[[1]],
            ";color:", item$text_color[[1]], ";"
          )
        ),
        shiny::span(
          item$remote_symbol[[1]],
          title = if (item$out_of_date[[1]]) {
            paste0("Newer repository version available: ", item$remote_status[[1]])
          } else {
            "No newer repository version"
          },
          class = "badge",
          style = paste0(
            "background:", item$remote_color[[1]],
            ";color:", item$remote_text_color[[1]], ";"
          )
        )
      )

      if (file_status %in% c("deleted", "missing")) {
        content <- tryCatch(
          svnOutput("diff", "--", file_path),
          error = function(e) ""
        )
        if (!nzchar(content)) content <- "The local file is missing or deleted."
        return(shiny::tagList(
          heading,
          shiny::tags$pre(
            class = "p-3 border rounded bg-light", style = "white-space:pre-wrap;",
            content
          )
        ))
      }

      if (is_figure) {
        figure_asset <- function(src) {
          if (extension == "png") {
            shiny::tags$img(src = src, style = "max-width:100%; height:auto;")
          } else {
            shiny::tags$embed(
              src = src, type = "application/pdf",
              style = "width:100%; height:calc(100vh - 230px);"
            )
          }
        }
        caption <- function(text, role) {
          style <- if (role == "prior") {
            "color:#991b1b; background:#fee2e2; border:1px solid #fca5a5; border-radius:6px; padding:4px 10px;"
          } else {
            "color:#166534; background:#dcfce7; border:1px solid #86efac; border-radius:6px; padding:4px 10px;"
          }
          shiny::div(class = "mb-2 fw-semibold", style = style, text)
        }

        local_src <- utils::URLencode(file.path("svn-local", relative_path))
        local_panel <- shiny::div(
          class = "border rounded p-3",
          caption("Local", "newer"),
          figure_asset(local_src)
        )
        if (is_new) return(shiny::tagList(heading, local_panel))

        revision <- svnInfo(file_path)[["rev"]]
        revision_file <- svnExport(
          .file = file_path, .revision = revision,
          .output_dir = revision_dir, .return_file = TRUE, .quiet = TRUE
        )
        revision_src <- utils::URLencode(
          file.path("svn-revisions", basename(revision_file))
        )
        revision_panel <- shiny::div(
          class = "border rounded p-3",
          caption(paste("Revision", revision), "prior"),
          figure_asset(revision_src)
        )
        local_comparison <- shiny::fluidRow(
          shiny::column(6, revision_panel),
          shiny::column(6, local_panel)
        )

        if (!item$out_of_date[[1]]) {
          return(shiny::tagList(heading, local_comparison))
        }

        repository_file <- svnExport(
          .file = file_path, .revision = "HEAD",
          .output_dir = revision_dir, .return_file = TRUE, .quiet = TRUE
        )
        repository_src <- utils::URLencode(
          file.path("svn-revisions", basename(repository_file))
        )
        repository_panel <- shiny::div(
          class = "border rounded p-3",
          caption("Repository HEAD", "newer"),
          figure_asset(repository_src)
        )
        incoming_comparison <- shiny::fluidRow(
          shiny::column(6, revision_panel),
          shiny::column(6, repository_panel)
        )

        if (file_status %in% c("modified", "replaced")) {
          return(shiny::tagList(
            heading,
            shiny::tabsetPanel(
              selected = "Incoming changes",
              shiny::tabPanel("Incoming changes", incoming_comparison),
              shiny::tabPanel("Local changes", local_comparison)
            )
          ))
        }

        return(shiny::tagList(heading, incoming_comparison))
      }

      if (is_new) {
        content <- if (fs::is_dir(file_path)) {
          "Directory contents are not previewed."
        } else if (!file.exists(file_path)) {
          "The local file does not exist."
        } else {
          paste(readLines(file_path, warn = FALSE), collapse = "\n")
        }
        return(shiny::tagList(
          heading,
          shiny::tags$pre(
            class = "p-3 border rounded bg-light", style = "white-space:pre-wrap;",
            content
          )
        ))
      }

      revision <- svnInfo(file_path)[["rev"]]
      local_diff <- diffPreviousRevisions(
        .file = file_path,
        .previous_revision = revision
      ) %>% suppressMessages()

      render_diff <- function(diff, empty_message) {
        if (is.null(diff)) {
          return(shiny::div(class = "alert alert-info", empty_message))
        }
        html <- paste(as.character(diff), collapse = "\n")
        shiny::tags$iframe(
          srcdoc = html,
          style = "width:100%; height:calc(100vh - 190px); border:1px solid #dee2e6;",
          sandbox = ""
        )
      }
      local_view <- render_diff(local_diff, "No local textual differences found.")

      if (!item$out_of_date[[1]]) {
        return(shiny::tagList(heading, local_view))
      }

      base_file <- svnExport(
        .file = file_path, .revision = revision,
        .output_dir = revision_dir, .return_file = TRUE, .quiet = TRUE
      )
      repository_file <- svnExport(
        .file = file_path, .revision = "HEAD",
        .output_dir = revision_dir, .return_file = TRUE, .quiet = TRUE
      )
      incoming_diff <- if (
        tools::md5sum(base_file) == tools::md5sum(repository_file)
      ) {
        NULL
      } else {
        diffFiles(
          .file_1 = base_file,
          .file_2 = repository_file,
          .banner_1 = paste0(basename(file_path), ": Revision ", revision),
          .banner_2 = paste0(basename(file_path), ": Repository HEAD")
        ) %>% suppressMessages()
      }
      incoming_view <- render_diff(
        incoming_diff,
        "No incoming textual differences found."
      )

      if (file_status %in% c("modified", "replaced")) {
        return(shiny::tagList(
          heading,
          shiny::tabsetPanel(
            selected = "Incoming changes",
            shiny::tabPanel("Incoming changes", incoming_view),
            shiny::tabPanel("Local changes", local_view)
          )
        ))
      }

      shiny::tagList(heading, incoming_view)
    })
  }

  shiny::shinyApp(
    ui,
    server,
    options = list(launch.browser = TRUE, quiet = TRUE)
  )
}
