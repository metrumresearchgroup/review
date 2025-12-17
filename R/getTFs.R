#' Extract file path of tables and figures
#' 
#' @description
#' Parses a PDF and extracts the file paths printed in lines beginning with
#' \code{Source graphic:} (figures) and \code{Source file:} (tables), along with
#' the most recently encountered \code{Source code:} path. Page numbers are
#' retained to help trace where each entry was found.
#' 
#' @param .file File path of PDF containing tables and figures
#' 
#' @examples
#' \dontrun{
#'   getTFs(here::here("deliv/report/pk-report.pdf"))
#' }
#' @export
getTFs <- function(.file) {
  
  # Read PDF into lines
  pdf.text_orig <- pdftools::pdf_text(.file)
  
  # Split into list to preserve page structure temporarily
  pdf.list <- strsplit(pdf.text_orig, "\n")
  
  # Create a map: repeats the page number for every line on that page
  page_map <- rep(seq_along(pdf.list), lengths(pdf.list))
  
  # Now flatten into a single vector of lines
  pdf.text <- unlist(pdf.list)
  pdf.text <- trimws(pdf.text)
  
  # Identify lines
  fig_idx   <- grep("^Source graphic:", pdf.text)
  tab_idx   <- grep("^Source file:", pdf.text)
  code_idx  <- grep("^Source code:", pdf.text)
  
  # Extract text for each
  figs  <- pdf.text[fig_idx]
  tabs  <- pdf.text[tab_idx]
  codes <- pdf.text[code_idx]
  
  figs_w_idx <-
    dplyr::tibble(
      idx = fig_idx,
      path = figs,
      page = page_map[fig_idx],
      type = "figure"
    )
  
  tabs_w_idx <-
    dplyr::tibble(
      idx = tab_idx,
      path = tabs,
      page = page_map[tab_idx],
      type = "table"
    )
  
  code_w_idx <-
    dplyr::tibble(
      idx = code_idx,
      code_path = codes,
      page = page_map[code_idx],
      type = "code"
    )
  
  combine1 <-
    dplyr::bind_rows(figs_w_idx, tabs_w_idx, code_w_idx) %>% 
    dplyr::arrange(idx) %>% 
    dplyr::select(page, type, dplyr::everything())
  
  combine2 <-
    combine1 %>% 
    tidyr::fill(code_path, .direction = "down") %>% 
    dplyr::filter(!is.na(path))
  
  df <- combine2
  
  # Remove prefixes for cleaner output (optional)
  df$path <- sub("^Source graphic:\\s*", "", df$path)
  df$path <- sub("^Source file:\\s*", "", df$path)
  df$code_path   <- sub("^Source code:\\s*", "", df$code_path)
  df$code_path <- gsub("\u2212", "-", df$code_path)
  df$path      <- gsub("\u2212", "-", df$path)
  df$idx <- NULL
  
  df
}