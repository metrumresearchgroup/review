#' @noRd
getPreviousCurrent <- function(.file, .previous_revision, .current_revision){
  
  .previous_revision_temp_dir <- tempdir()
  
  .previous_revision_temp_file <- 
    svnExport(
      .file = .file, 
      .revision = .previous_revision, 
      .output_dir = .previous_revision_temp_dir, 
      .return_file = TRUE,
      .quiet = TRUE
    )
  
  .previous_revision_header <- glue::glue("{basename(.file)}: Revision {.previous_revision}")
  
  if (is.null(.current_revision)) {
    
    .current_revision_temp_file <- tempfile(fileext = glue::glue(".{tools::file_ext(.file)}"))
    
    system(glue::glue("cp {.file} {.current_revision_temp_file}"))
    
    .current_revision_header <- glue::glue("{basename(.file)}: Local")
    
  } else {
    
    .current_revision_temp_dir <- tempdir()
    
    .current_revision_temp_file <- 
      svnExport(
        .file = .file, 
        .revision = .current_revision, 
        .output_dir = .current_revision_temp_dir, 
        .return_file = TRUE,
        .quiet = TRUE
      )
    
    .current_revision_header <- glue::glue("{basename(.file)}: Revision {.current_revision}")
    
  }
  
  list(
    .previous_revision_temp_file = .previous_revision_temp_file,
    .previous_revision_header = .previous_revision_header,
    .current_revision_temp_file = .current_revision_temp_file,
    .current_revision_header = .current_revision_header
  )
  
}