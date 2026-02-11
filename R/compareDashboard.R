#' Launch an interactive figure revision dashboard
#'
#' @description
#' `compareDashboard()` opens a Shiny app that lets you select a figure (PDF/PNG)
#' from a folder and compare any two SVN revisions. The app shows the older and
#' newer revisions side by side.
#'
#' @param .path `character(1)`
#'   Folder containing figures tracked in SVN.
#'
#' @examples
#' \dontrun{
#' # Review figure revisions interactively
#' compareDashboard("deliv/figure")
#' }
#'
#' @export
compareDashboard <- function(.path) {
  if (!fs::is_dir(.path)) {
    stop("`.path` must be a directory containing figure files.")
  }

  fig_files <- list.files(
    .path,
    pattern = "\\.(pdf|png)$",
    ignore.case = TRUE,
    full.names = TRUE
  )

  if (!length(fig_files)) {
    stop("No PDF or PNG figures found in the supplied folder.")
  }

  fig_files <- fs::path_abs(fig_files)
  fig_names <- basename(fig_files)
  fig_map <- stats::setNames(fig_files, fig_names)

  fig_dir <- fs::path_abs(.path)
  rev_dir <- file.path(tempdir(), "review-figure-dashboard")
  dir.create(rev_dir, showWarnings = FALSE, recursive = TRUE)

  shiny::addResourcePath("figures", fig_dir)
  shiny::addResourcePath("revisions", rev_dir)

  get_revision_file <- function(.file, .revision, .rev_dir) {
    if (identical(.revision, "Local") || is.null(.revision)) {
      return(.file)
    }
    svnExport(
      .file = .file,
      .revision = .revision,
      .output_dir = .rev_dir,
      .return_file = TRUE,
      .quiet = TRUE
    )
  }

  ui <- bslib::page_sidebar(
    title = dashboard_title("Figure Comparison:", .path),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    sidebar = bslib::sidebar(
      open = "always",
      width = 360,
      timeline_assets(
        "calc(100vh - 220px)",
        extra_css = "
        .fig-panel { border:1px solid #dee2e6; border-radius:6px; padding:12px; background:#fff; }
        .fig-caption { margin-bottom: 8px; font-weight: 600; }
        .fig-asset { width:100%; border:1px solid #e9ecef; }
        "
      ),
      shiny::selectInput("figure_file", "Figure", choices = fig_names),
      shiny::div(
        class = "text-muted mb-2",
        shiny::tags$i(
          "Click two revisions to compare side by side.",
          style = "font-size: smaller;"
        )
      ),
      shiny::div(class = "side-scroll", shiny::uiOutput("timeline_ui"))
    ),
    shiny::uiOutput("figure_ui")
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() shiny::stopApp())

    show_app_exit_hint("compareDashboard")

    current_file <- shiny::reactive({
      fig_map[[input$figure_file]]
    })

    svn_log <- shiny::reactive({
      getRevHistory(.file = pathFromLogRoot(current_file()))
    })

    selection <- shiny::reactiveVal(list(ids = character(), prior = NULL, newer = NULL))

    shiny::observeEvent(
      current_file(),
      {
        sv <- svn_log()
        available <- as.character(sv$rev)
        current_ids <- selection()$ids
        has_same <- length(current_ids) >= 2L && all(current_ids %in% available)
        if (has_same) {
          selection(compute_selection(current_ids))
        } else {
          default_sel <- default_selection_from_log(sv)
          selection(compute_selection(default_sel))
        }
      },
      ignoreInit = FALSE
    )

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

    output$timeline_ui <- shiny::renderUI({
      chosen <- selection()$ids
      sv <- svn_log()

      render_timeline(sv, chosen)
    })

    output$figure_ui <- shiny::renderUI({
      p <- selection()
      if (is.null(p$prior)) {
        return(shiny::tags$div(
          style = "padding: 20px; text-align: center; color: #004085; background-color: #cce5ff; border: 1px solid #b8daff; border-radius: 6px;",
          "Select two revisions to display a figure."
        ))
      }

      render_panel <- function(.rev, .label) {
        file_path <- current_file()
        rev_file <- get_revision_file(file_path, .rev, rev_dir)
        ext <- tolower(tools::file_ext(rev_file))
        src <- if (identical(.rev, "Local")) {
          file.path("figures", basename(file_path))
        } else {
          file.path("revisions", basename(rev_file))
        }
        src <- utils::URLencode(src)

        caption <- if (identical(.rev, "Local")) {
          paste0(.label, " (Local)")
        } else {
          paste0(.label, " (Rev: ", .rev, ")")
        }

        asset <- if (ext == "png") {
          shiny::tags$img(
            src = src,
            style = "max-width:100%; height:auto;",
            class = "fig-asset"
          )
        } else {
          shiny::tags$embed(
            src = src,
            type = "application/pdf",
            style = "height: calc(100vh - 260px);",
            class = "fig-asset"
          )
        }

        shiny::tags$div(
          class = "fig-panel",
          shiny::tags$div(class = "fig-caption", caption),
          asset
        )
      }

      newer_rev <- if (is.null(p$newer)) "Local" else p$newer

      shiny::fluidRow(
        shiny::column(6, render_panel(p$prior, "Older")),
        shiny::column(6, render_panel(newer_rev, "Newer"))
      )
    })
  }

  shiny::shinyApp(
    ui,
    server,
    options = list(launch.browser = TRUE, quiet = TRUE)
  )
}
