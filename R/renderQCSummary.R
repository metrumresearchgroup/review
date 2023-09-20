#' @export
renderQCSummary <- function(.dir, .output_dir = NULL) {
  
  dirSummaryRes <- dirSummary(.dir)
  
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