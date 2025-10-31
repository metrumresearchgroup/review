#' Generate a list of all tables and figures in a PDF
#' 
#' @description
#' This function parses a PDF containing tables and figures
#' saved by `pmtables` and `mrggsave`. It will return a list
#' of all tables and figures included in the PDF provided. 
#' 
#' @param .file File path 
#' 
#' @export
getTFLs <- function(.file) {
  pdf.text <- pdftools::pdf_text(.file)
  pdf.text <- unlist(strsplit(pdf.text, "\n"))
  source_figures <- pdf.text[grep("Source graphic:", pdf.text)]
  source_figures <- unique(trimws(source_figures))
  source_tables <- pdf.text[grep("Source file:", pdf.text)]
  source_tables <- unique(trimws(source_tables))
  sources <- c(source_figures, source_tables)
  sources
}
