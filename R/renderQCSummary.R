#' Render a QC Summary Document
#'
#' This function generates a QC summary based on scripts within a given project 
#' and renders it into a PDF report. 
#' 
#' The QC status of all scripts checked into SVN by each author will be displayed, 
#' along with a high level summary of all scripts that need to be QCed.
#'
#' @param .output_dir Character string (optional). Path to the directory where the output PDF 
#'   should be saved. If not provided, the document will not be saved locally.
#'
#' @return Invisible. The function will save a PDF report named "qc-summary-(current date).pdf"
#'   to the specified output directory (if available). If the R session is interactive, it will also 
#'   open the PDF in the default browser.
#'
#' @export
renderQCSummary <- function(.output_dir = NULL) {
  
  dirSummaryRes <- dirSummary()
  
  if (is.null(.output_dir)) {
    .output_dir <- tempdir()
  } else {
    if (!dir.exists(.output_dir)) {
      stop(.output_dir, " does not exist")
    }
  }
  
  output_file <- paste0("qc-summary-", Sys.Date(), ".pdf")
  output_path <- file.path(.output_dir, output_file)
  
  rmarkdown::render(
    input = system.file("templates", "QCSummary.Rmd", package = "review"),
    output_file = output_path,
    params = list(dirSummaryRes = dirSummaryRes),
    envir = new.env(),
    quiet = TRUE
  )
  
  if (interactive()) {
    utils::browseURL(output_path)
  }
  
}
