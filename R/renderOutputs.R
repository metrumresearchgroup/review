#' Render an Output Mapping Report
#'
#' This function generates a report showing every file that is output from scripts
#' in the repo. Only files that have been run with `runWithOutputs()` will appear
#' in the table. 
#' 
#' Additionally, there is a check to confirm if the outputs are up to date with 
#' the latest version of the source script.
#'
#' @param .output_dir Character string. Path to the directory where the output PDF
#'   should be saved. This argument is required.
#'
#' @param .project_number Character string (optional). Used for the output PDF filename
#'   prefix. If NULL (default), inferred from the SVN project URL.
#'   
#' @examples 
#' \dontrun{
#' renderOutputs("deliv/report")
#' }
#'
#' @export
renderOutputs <- function(.output_dir, .project_number = NULL) {
  
  # Check if QC log is missing
  if (is.null(logRoot())) {
    stop("QC log does not exist")
  }
  
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
    .project_number <- utils::tail(strsplit(project_id, split = "-", fixed = TRUE)[[1]], 1)
  }
  
  output_file <- paste0(tolower(.project_number), "-file-outputs-report-", Sys.Date(), ".pdf")
  output_path <- file.path(.output_dir, output_file)
  
  params_in <- list(
    project = project_id,
    outputsDF = formatOutputs()
  )
  
  rmarkdown::render(
    input = system.file("templates", "OutputsReport.Rmd", package = "review"),
    output_file = output_path,
    params = params_in,
    envir = new.env(),
    quiet = TRUE
  )
  
  if (interactive()) {
    utils::browseURL(output_path)
  }
  
}
