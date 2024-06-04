#' Generate comparison between sets of figures
#' 
#' @description 
#' Generate a html document showing two versions of a figure (pdf or png).
#' If two paths are provided, the local versions of both will be used.
#' If only one path is provided, the comparison will show the local 
#' versus last checked in version.
#' 
#' @param .path_current file or directory path to figures of interest
#' @param .path_previous file or directory path to compare .path_base to (leave as NULL to compare to repo version)
#' @param .file_types file extensions to include in comparison (only pdf and png allowed)
#' @param .side_by_side Logical. Should outputs be displayed side by side?
#' @param .show_on_load Logical. Should all figures be open when the page loads?
#' 
#' @export
compareFigures <- function(.path_current, 
                           .path_previous = NULL,
                           .file_types = "pdf",
                           .side_by_side = TRUE,
                           .show_on_load = TRUE) {
  
  .allowed_exts <- c("png", "pdf")
  
  if (!all(.file_types %in% .allowed_exts)) {
    stop("Only file extensions ", paste(.allowed_exts, collapse = ", "), " are allowed")
  }
  
  cli::cli_alert(paste0("Checking '", cli::col_blue(.path_current), "' for files of type(s): ", cli::col_green(paste(.file_types, collapse = ", ")), "\n"))
  
  if (is.null(.path_previous)) {
    compareModified(
      .path = .path_current, 
      .side_by_side = .side_by_side, 
      .file_exts = .file_types,
      .show_on_load = .show_on_load
    )
  } else {
    compareLocal(
      .path_base = .path_previous, 
      .path_compare = .path_current, 
      .side_by_side = .side_by_side,
      .file_exts = .file_types,
      .show_on_load = .show_on_load
    )
  }
  
}
