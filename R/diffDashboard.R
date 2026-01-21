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
          style = "padding: 20px; text-align: center; color: #155724; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 6px;",
          shiny::HTML(
            sprintf(
              "The two versions are identical.<br/><span style='font-weight: 600;'>Compared <span style='padding:2px 6px; background:#e2e3e5; border-radius:4px;'>%s</span> to <span style='padding:2px 6px; background:#e2e3e5; border-radius:4px;'>%s</span>.</span>",
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
