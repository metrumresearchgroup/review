#' Launch an interactive figure revision dashboard
#'
#' @description
#' `figureDashboard()` opens a Shiny app that lets you select a figure (PDF/PNG)
#' from a folder and compare any two SVN revisions. The app shows the older and
#' newer revisions side by side.
#'
#' @param .path `character(1)`
#'   Folder containing figures tracked in SVN.
#'
#' @examples
#' \dontrun{
#' # Review figure revisions interactively
#' figureDashboard("deliv/figure")
#' }
#'
#' @export
figureDashboard <- function(.path) {
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
    title = htmltools::div(
      style = "display:flex; gap:.35rem; align-items:baseline; font-weight:600;",
      htmltools::span("Figure Comparison:"),
      htmltools::span(
        style = "opacity:.7; font-weight:400;",
        fs::path_rel(.path)
      )
    ),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    sidebar = bslib::sidebar(
      open = "always",
      width = 360,
      shiny::tags$style(htmltools::HTML(
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
        .side-scroll { max-height: calc(100vh - 220px); overflow:auto; padding-right:4px; }
        .fig-panel { border:1px solid #dee2e6; border-radius:6px; padding:12px; background:#fff; }
        .fig-caption { margin-bottom: 8px; font-weight: 600; }
        .fig-asset { width:100%; border:1px solid #e9ecef; }
      "
      )),
      shiny::tags$script(htmltools::HTML(
        "
        document.addEventListener('click', function(e){
          var el = e.target.closest('.rev'); if(!el) return;
          var r = el.getAttribute('data-rev');
          if(window.Shiny && r) Shiny.setInputValue('rev_clicked', r, {priority:'event'});
        }, true);
      "
      )),
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

    show_app_exit_hint("figureDashboard")

    current_file <- shiny::reactive({
      fig_map[[input$figure_file]]
    })

    svn_log <- shiny::reactive({
      getRevHistory(.file = current_file())
    })

    selection <- shiny::reactiveVal(list(ids = character(), prior = NULL, newer = NULL))

    shiny::observeEvent(
      current_file(),
      {
        sv <- svn_log()
        revisions <- sv$rev
        newest <- max(as.numeric(revisions[revisions != "Local"]), na.rm = TRUE)
        default_sel <- c(as.character(newest), "Local")
        selection(compute_selection(default_sel))
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

      rev_items <- lapply(seq_len(nrow(sv)), function(i) {
        row <- sv[i, ]
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
    })

    output$figure_ui <- shiny::renderUI({
      p <- selection()
      if (is.null(p$prior)) {
        return(shiny::tags$div(
          style = "padding: 20px; text-align: center; color: #6c757d;",
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
