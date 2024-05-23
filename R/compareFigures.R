#' Generate comparison between sets of figures
#' 
#' @description 
#' Generate a html document showing two versions of a figure (pdf or png).
#' If two paths are provided, the local versions of both will be used.
#' If only one path is provided, the comparison will show the local 
#' versus last checked in version.
#' 
#' @param .path_current file or directory path to figures of interest
#' @param .path_previous file or directory path to compare .path_base to
#' @param .file_exts file extensions to include in comparison (only pdf and png allowed)
#' @param .side_by_side Logical. Should outputs be displayed side by side?
#' 
#' @export
compareFigures <- function(.path_current, 
                           .path_previous = NULL,
                           .file_exts = c("png", "pdf"),
                           .side_by_side = TRUE) {
  
  if (is.null(.path_previous)) {
    compareModified(
      .path = .path_current, 
      .side_by_side = .side_by_side, 
      .file_exts = .file_exts
    )
  } else {
    compareLocal(
      .path_base = .path_current, 
      .path_compare = .path_previous, 
      .side_by_side = .side_by_side,
      .file_exts = .file_exts
    )
  }
  
}
  