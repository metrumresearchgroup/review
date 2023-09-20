#' @export
renderSummary <- function(.dir) {
  
  summary <- dirSummary(.dir)
  output_path <- tempfile(fileext = ".pdf")
  
  rmarkdown::render(
    input = system.file("templates/renderSummary.Rmd", package = "review"),
    output_file = output_path,
    params = list(dirSum = summary),
    envir = new.env(),
    quiet = TRUE
  )
  
  if (interactive()) {
    utils::browseURL(output_path)
  }
  
}