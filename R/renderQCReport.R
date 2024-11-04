#' Render a QC Report Document
#'
#' This function generates a QC report based on the files within the QC log
#' and renders it into a PDF report.
#' 
#' The QC status of all files in the QC log will be displayed.
#' 
#' @param .output_dir Character string (optional). Path to the directory where the output PDF 
#'   should be saved. If not provided, the document will not be saved locally.
#'
#' @return Invisible. The function will save a PDF report named "qc-report-(current date).pdf"
#'   to the specified output directory (if available). If the R session is interactive, it will also 
#'   open the PDF in the default browser.
#'   
#' @examples 
#' \dontrun{
#' renderQCReport()
#' }
#'
#' @export
renderQCReport <- function(.output_dir = NULL) {
  
  if (is.null(.output_dir)) {
    .output_dir <- tempdir()
  } else {
    if (!dir.exists(.output_dir)) {
      stop(.output_dir, " does not exist")
    }
  }
  
  output_file <- paste0("qc-report-", Sys.Date(), ".pdf")
  output_path <- file.path(.output_dir, output_file)
  
  params_in <- list(
    project = basename(logRoot()),
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
