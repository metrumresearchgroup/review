#' Render a QC Report Document
#'
#' This function generates a QC report based on the files within the QC log
#' and renders it into a PDF report.
#' 
#' The QC status of all files in the QC log will be displayed.
#'
#' @param .output_dir Character string. Path to the directory where the output PDF
#'   should be saved. This argument is required.
#'
#' @param .project_number Character string (optional). Used for the output PDF filename
#'   prefix. If NULL (default), inferred from the SVN project URL.
#'
#' @return Invisible. The function will save a PDF report named "<project>-qc-report-(current date).pdf"
#'   to the specified output directory. If the R session is interactive, it will also
#'   open the PDF in the default browser.
#'   
#' @examples 
#' \dontrun{
#' renderQCReport("deliv/report")
#' }
#'
#' @export
renderQCReport <- function(.output_dir, .project_number = NULL) {
  
  if (missing(.output_dir) || !is.character(.output_dir)) {
    stop("'.output_dir' is required and must be a character string.")
  }
  
  .output_dir <- as.character(fs::path_abs(.output_dir))
  
  if (!dir.exists(.output_dir)) {
    stop(.output_dir, " does not exist")
  }
  
  # Always extract project_id from the last part after the final slash in the SVN URL
  # ".*/([^/]+)$" = match everything up to the last slash and capture what comes after it
  # "\\1" = replace with the captured group (the part after the last slash)
  projInfo <- svnProjInfo()
  project_id <- sub(".*/([^/]+)$", "\\1", projInfo$url)

  # report_suffix for filename: .project_number if supplied, else SVN last segment
  if (is.null(.project_number)) {
    .project_number <- tail(strsplit(project_id, split = "-", fixed = TRUE)[[1]], 1)
  }
  
  output_file <- paste0(tolower(.project_number), "-qc-report-", Sys.Date(), ".pdf")
  output_path <- file.path(.output_dir, output_file)
  
  params_in <- list(
    project = project_id, # Always from SVN
    logSum = logSummary()
  )
  
  rmarkdown::render(
    input = system.file("templates", "QCReport.Rmd", package = "review"),
    output_file = output_path,
    params = params_in,
    envir = new.env(),
    quiet = TRUE
  )
  
  if (interactive()) {
    utils::browseURL(output_path)
  }
  
}
