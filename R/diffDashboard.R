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

  revisions <- svn_log$rev
  newest <- max(revisions, na.rm = TRUE)

  # --- UI ---
  ui <- bslib::page_sidebar(
    title = htmltools::div(
      style = "display:flex; gap:.35rem; align-items:baseline; font-weight:600;",
      htmltools::span("Visual diff:"),
      htmltools::span(
        style = "opacity:.7; font-weight:400;",
        fs::path_rel(.file)
      )
    ),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    sidebar = bslib::sidebar(
      open = "always",
      width = 360,
      # minimal styles and a simple click handler
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
        .side-scroll { max-height: calc(100vh - 160px); overflow:auto; padding-right:4px; }
      "
      )),
      shiny::tags$script(htmltools::HTML(
        "
        document.addEventListener('click', function(e){
          var el = e.target.closest('.rev'); if(!el) return;
          var r = el.getAttribute('data-rev'); // could be 'LOCAL' or a number string
          if(window.Shiny && r) Shiny.setInputValue('rev_clicked', r, {priority:'event'});
        }, true);
      "
      )),
      shiny::div(
        class = "text-muted mb-2",
        "Click any two to compare:"
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

    # Selected IDs (strings): numeric revisions as strings + 'LOCAL'
    # Default to newest vs LOCAL for immediate utility
    default_sel <- c(as.character(newest), "LOCAL")
    sel <- shiny::reactiveVal(default_sel)

    shiny::observeEvent(
      input$rev_clicked,
      {
        r <- as.character(input$rev_clicked)
        cur <- sel()
        if (r %in% cur) {
          sel(setdiff(cur, r))
        } else {
          new <- c(cur, r)
          new <- new[!duplicated(new)]
          if (length(new) > 2) {
            new <- utils::tail(new, 2)
          }
          sel(new)
        }
      },
      ignoreInit = TRUE
    )

    # Pairing logic: if LOCAL is present, force it to be `newer`
    picked <- shiny::reactive({
      x <- sel()
      if (length(x) < 2) {
        return(NULL)
      }
      if ("LOCAL" %in% x) {
        other <- setdiff(x, "LOCAL")
        if (!length(other)) {
          return(NULL)
        }
        # choose the numeric rev as prior (use min in case of oddities)
        other_num <- suppressWarnings(as.numeric(other))
        other_num <- other_num[!is.na(other_num)]
        if (!length(other_num)) {
          return(NULL)
        }
        list(prior = min(other_num), newer = NULL)
      } else {
        # both numeric
        nums <- suppressWarnings(as.numeric(x))
        nums <- sort(nums)
        if (length(nums) < 2) {
          return(NULL)
        }
        list(prior = nums[1], newer = nums[2])
      }
    })

    # --- Timeline UI (LOCAL first, then SVN revisions) ---
    output$timeline_ui <- shiny::renderUI({
      chosen <- sel()

      local_item <- {
        cls <- if ("LOCAL" %in% chosen) "rev sel" else "rev"
        shiny::div(
          class = cls,
          `data-rev` = "LOCAL",
          shiny::div(
            class = "h",
            shiny::span(class = "id", "Local"),
            shiny::span(class = "badge local", "Working copy"),
            shiny::span(
              class = "m",
              paste(
                paste0(Sys.info()[["user"]], " :"),
                format(file.info(.file)$mtime, "%Y-%m-%d %H:%M:%S")
              )
            )
          ),
          shiny::div(
            class = "msg",
            shiny::tags$em("Working copy")
          )
        )
      }

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
            shiny::span(class = "id", paste0("r", row$rev)),
            qc,
            shiny::span(class = "m", paste(row$author, ":", row$elapsed))
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

      shiny::div(class = "timeline", local_item, rev_items)
    })

    output$diff_html <- shiny::renderUI({
      p <- picked()
      shiny::req(p)

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
        shiny::tags$div(
          style = "padding: 20px; text-align: center; color: #155724; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 6px;",
          "The two versions are identical."
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
