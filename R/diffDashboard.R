#' Launch an interactive visual diff dashboard for a file's revision/QC history
#'
#' @description
#' `diffDashboard()` opens a Shiny app that helps you review a file's Subversion
#' (SVN) revision history and QC status, and interactively compare any two
#' versions (including your local working copy) with a visual diff.
#'
#' @param .file `character(1)`
#'   Path to the target file tracked in SVN. Can be absolute or relative.
#'
#' @examples
#' \dontrun{
#' # Review and diff revisions for a script
#' diffDashboard("analysis/model.R")
#' }
#'
#' @export
diffDashboard <- function(.file) {
  # --- Data prep ---
  svn_log <- getRevHistory(.file = .file)
  qced_revision <- getQcedRevision(.file)
  latest_revision <- max(as.numeric(svn_log$rev[svn_log$rev != "Local"]), na.rm = TRUE)

  default_sel <- default_selection_from_log(svn_log)

  # --- UI ---
  ui <- bslib::page_sidebar(
    title = dashboard_title("Revision Comparison:", .file),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    sidebar = bslib::sidebar(
      open = "always",
      width = 360,
      # minimal styles and a simple click handler
      timeline_assets("calc(100vh - 160px)"),
      shiny::div(
        class = "text-muted mb-2",
        shiny::tags$i(
          "Click two to compare. Older is red, newer is green.",
          style = "font-size: smaller;"
        )
      ),
      shiny::uiOutput("quick_actions"),
      shiny::div(class = "side-scroll", shiny::uiOutput("timeline_ui"))
    ),

    shiny::fluidRow(
      class = "mb-3",
      shiny::column(
        4,
        shiny::checkboxInput("side_by_side", "Side-by-side view", value = TRUE)
      ),
      shiny::column(
        4,
        shiny::checkboxInput("ignore_ws", "Ignore white space", value = FALSE)
      ),
      shiny::column(
        4,
        shiny::checkboxInput(
          "display_entire_file",
          "Display entire file",
          value = FALSE
        )
      )
    ),

    # Main content
    shiny::uiOutput("diff_html")
  )

  # --- SERVER ---
  server <- function(input, output, session) {
    session$onSessionEnded(function() shiny::stopApp())

    # Unified selection state using extracted helpers
    selection <- shiny::reactiveVal(compute_selection(default_sel))

    # show hint once the server has started
    show_app_exit_hint("diffDashboard")

    shiny::observeEvent(
      input$rev_clicked,
      {
        new_ids <- update_selection(
          selection()$ids,
          input$rev_clicked,
          max_sel = 2L
        )
        selection(compute_selection(new_ids))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$jump_qc_local,
      {
        selection(compute_selection(c(as.character(qced_revision), "Local")))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$jump_latest_local,
      {
        selection(compute_selection(default_sel))
      },
      ignoreInit = TRUE
    )

    output$quick_actions <- shiny::renderUI({
      current <- selection()

      base_style <- paste0(
        "width:100%; white-space:normal;",
        " background-color:#3f4348; border-color:#3f4348; color:#ffffff;"
      )
      disabled_style <- paste0(
        base_style,
        " background-color:#f3f4f6; border-color:#d1d5db; color:#9ca3af;",
        " box-shadow:none; font-weight:400; cursor:not-allowed;"
      )
      active_style <- paste0(
        base_style,
        " border-color:#8fb4ff;",
        " box-shadow:0 0 0 5px rgba(44,123,229,0.20), inset 0 0 0 1px rgba(143,180,255,0.55);",
        " font-weight:600;"
      )

      latest_active <- isTRUE(
        !is.null(current$prior) &&
          is.null(current$newer) &&
          !is.na(latest_revision) &&
          current$prior == latest_revision
      )

      qc_active <- FALSE
      if (!is.na(qced_revision)) {
        qc_active <- isTRUE(
          !is.null(current$prior) &&
            is.null(current$newer) &&
            current$prior == qced_revision
        )
      }

      shiny::div(
        style = "margin-top:-18px; margin-bottom:12px;",
        shiny::div(
          class = "text-muted",
          style = "font-size:0.8rem; text-transform:uppercase; letter-spacing:0.04em; margin-bottom:2px;",
          "Quick actions"
        )
      ,
        shiny::div(
          style = "display:grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap:8px;",
          shiny::actionButton(
            "jump_qc_local",
            "Last QC -> Local",
            style = if (is.na(qced_revision)) disabled_style else if (qc_active) active_style else base_style,
            disabled = if (is.na(qced_revision)) "disabled" else NULL
          ),
          shiny::actionButton(
            "jump_latest_local",
            "Latest SVN -> Local",
            style = if (latest_active) active_style else base_style
          )
        )
      )
    })

    # --- Timeline UI (LOCAL first, then SVN revisions) ---
    output$timeline_ui <- shiny::renderUI({
      chosen <- selection()$ids

      render_timeline(svn_log, chosen)
    })

    output$diff_html <- shiny::renderUI({
      p <- selection()
      shiny::req(!is.null(p$prior))

      sbs <- shiny::isTruthy(input$side_by_side)
      igw <- shiny::isTruthy(input$ignore_ws)
      def <- shiny::isTruthy(input$display_entire_file)

      # Compute HTML diff
      diff_obj <- diffPreviousRevisions(
        .file = .file,
        .previous_revision = p$prior,
        .current_revision = p$newer,
        .side_by_side = sbs,
        .ignore_white_space = igw,
        .display_entire_file = def
      ) %>%
        suppressMessages()

      if (is.null(diff_obj)) {
        # Case when there are no differences
        newer_label <- if (is.null(p$newer)) {
          "Local"
        } else {
          paste0("Rev: ", p$newer)
        }
        prior_label <- paste0("Rev: ", p$prior)

        shiny::tags$div(
          style = "padding: 20px; text-align: center; color: #004085; background-color: #cce5ff; border: 1px solid #b8daff; border-radius: 6px;",
          shiny::HTML(
            sprintf(
              "<span style='font-size: 1.1em; font-weight: 600;'>No differences found</span><br/><span style='font-size: 0.95em;'>%s and %s are identical.</span>",
              prior_label,
              newer_label
            )
          )
        )
      } else {
        # Case when there are differences
        html <- paste(as.character(diff_obj), collapse = "\n")
        shiny::tags$iframe(
          srcdoc = html,
          style = "width:100%; height: calc(100vh - 180px); border:1px solid #dee2e6;",
          sandbox = "allow-same-origin allow-forms allow-scripts allow-popups"
        )
      }
    })
  }

  shiny::shinyApp(
    ui,
    server,
    options = list(launch.browser = TRUE, quiet = TRUE)
  )
}
