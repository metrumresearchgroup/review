#' Export a specific revision of a file from SVN
#'
#' This function exports a specific revision of a file from SVN to a desired output directory.
#' The exported file will be named with its original base name followed by a hyphen and the revision number.
#' If the `.output_dir` is not specified, the current working directory will be used.
#'
#' @param .file A character string specifying the path to the SVN file to be exported.
#' @param .revision A character or numeric value specifying the SVN revision number of the file to be exported. (If NULL, the most recent version in the repository will be exported.)
#' @param .output_dir A character string specifying the directory where the exported file should be saved.
#'   Defaults to the current working directory.
#' @param .return_file Boolean. Should the newly created file name be returned as a string?
#' @param .quiet Boolean. Should messages be printed?
#' 
#' @examples
#' \dontrun{
#'   svnExport(.file = "/path/to/svn/file.txt", .revision = "1234", .output_dir = "/path/to/save")
#' }
#' 
#' @export
svnExport <- function(.file, .revision = NULL, .output_dir = getwd(), .return_file = FALSE, .quiet = FALSE) {
  
  if (!file.exists(.file)) {
    stop(".file not found")
  }
  
  if (is.null(.revision)) {
    .revision <- as.numeric(svnInfo(.file = .file)[["rev"]])
  }
  
  .file_rev_path <- 
    file.path(
      .output_dir,
      paste0(
        basename(tools::file_path_sans_ext(.file)),
        "-",
        .revision,
        ".",
        tools::file_ext(.file)
      )
    )
  
  # Add the single quotes in between the files
  # svnCommand will add the outer quotes
  .file_command <- paste0(.file, "' '", .file_rev_path)
  
  export_try <- tryCatch(
    svnCommand(
      .file = .file_command,
      .command = "export",
      .flags = paste0("--force -r", .revision),
      .xml = FALSE
    ),
    error = identity
  )
  
  if (inherits(export_try, "error")) {
    stop("svn export failed")
  }
  
  if (!.quiet) {
    cli::cli_inform(paste0("File exported: ", .file_rev_path))
  }
  
  if (.return_file) {
    return(.file_rev_path)
  }
  
  return(invisible(NULL))
  
}
