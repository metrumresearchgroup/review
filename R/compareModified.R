#' Generate comparison between local (modified) and checked in file(s)
#' 
#' @param .path file or directory path to tables/figures of interest (pdf, png, or tex)
#' 
#' @param .side_by_side Logical. Should outputs be displayed side by side?
#' 
#' @export
compareModified <- function(.path, .side_by_side = TRUE) {
  
  .exts <- c("png", "pdf", "tex")
  
  .dfpaths <- getModified(.path = .path, .exts = .exts)
  
  buildCompare(.dfpaths = .dfpaths, .side_by_side = .side_by_side)
}
