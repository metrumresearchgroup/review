#' @export
svnExport <- function(.file, .revision, .output_dir) {
  
  .file_command <- paste0(.file, )
  
  export_df <- tryCatch(
    svnCommand(
      .file = .file, 
      .command = "export",
      .flags = paste0("-r", .revision)),
    error = identity
  )
  
  if (inherits(export_df, "error")) {
    stop("svn export failed")
  }

}