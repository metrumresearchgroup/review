#' Generate comparison between local (modified) and checked in file(s)
#' 
#' @param .path file or directory path to tables/figures of interest
#' 
#' @param .side_by_side Logical. Should outputs be displayed side by side?
#' 
#' @param .file_exts file extensions to include in comparison
#' 
#' @noRd
compareModified <- function(.path, .side_by_side = TRUE, .file_exts = c("png", "pdf", "tex")) {
  
  .allowed_exts <- c("png", "pdf", "tex")
  
  if (!all(.file_exts %in% .allowed_exts)) {
    stop("Only file extensions ", paste(.allowed_exts, collapse = ", "), " are allowed")
  }
  
  .dfpaths <- getModified(.path = .path, .exts = .file_exts)
  
  buildCompare(
    .dfpaths = .dfpaths,
    .side_by_side = .side_by_side,
    .headings = c("Repo", "Local")
  )
}
