#' Extract file path of tables and figures
#' 
#' @description
#' Returns a data.frame with all tables and figures included in the provided
#' PDF. For each, the file path of both the source code and the file itself is 
#' recorded.
#' 
#' @param .file File path of PDF containing tables and figures
#' @export
getTFs <- function(.file) {
  
  # Read PDF into lines
  pdf.text <- pdftools::pdf_text(.file)
  pdf.text <- unlist(strsplit(pdf.text, "\n"))
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
      path = figs
    )
  
  tabs_w_idx <-
    dplyr::tibble(
      idx = tab_idx,
      path = tabs
    )
  
  code_w_idx <-
    dplyr::tibble(
      idx = code_idx,
      code_path = codes
    )
  
  combine1 <-
    dplyr::bind_rows(figs_w_idx, tabs_w_idx, code_w_idx) %>% 
    dplyr::arrange(idx)
  
  combine2 <-
    combine1 %>% 
    tidyr::fill("code_path", .direction = "down") %>% 
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