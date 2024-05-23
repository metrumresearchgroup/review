#' Generate comparison between sets of local file(s)
#' 
#' @param .path_base file or directory path to tables/figures of interest
#' 
#' @param .path_compare file or directory path to compare .path_base to
#' 
#' @param .side_by_side Logical. Should outputs be displayed side by side?
#' 
#' @param .file_exts file extensions to include in comparison
#' 
#' @noRd
compareLocal <- function(.path_base, .path_compare, .side_by_side = TRUE, .file_exts = c("png", "pdf", "tex")) {
   
  .allowed_exts <- c("png", "pdf", "tex")
  
  if (!all(.file_exts %in% .allowed_exts)) {
    stop("Only file extensions ", paste(.allowed_exts, collapse = ", "), " are allowed")
  }

  .is_dir <- fs::is_dir(.path_base)
  
  common_files <- 
    
    if (.is_dir) {
      
      intersect(list.files(.path_base), list.files(.path_compare))
      
    } else {
      
      c(basename(.path_base), basename(.path_compare))
      
    }
  
  common_files <- common_files[tools::file_ext(common_files) %in% .file_exts]
  
  if (length(common_files) == 0) {
    stop("No common files in .path_base and .path_compare of type(s) ", paste(.file_exts, collapse = ", "))
  }
  
  if (.is_dir) {
    
    .path1 <- file.path(fs::path_abs(.path_base), common_files)
    .path2 <- file.path(fs::path_abs(.path_compare), common_files)
    
  } else{
    
    .path1 <- fs::path_abs(.path_base)
    .path2 <- fs::path_abs(.path_compare)
    
  }
  
  figures_meta <- 
    data.frame(
      path1 = .path1, 
      path2 = .path2
    )
  
  figures_meta[["compname"]] <- basename(figures_meta[["path1"]])
  
  figures_meta$mtime1 <- NA_real_
  figures_meta$mtime2 <- NA_real_
  
  for (i in 1:nrow(figures_meta)) {
    
    cli::cli_alert(paste0("Comparing: ", cli::col_blue(figures_meta$compname[i])))
    
    figures_meta$mtime1[i] <- as.POSIXct(format(file.info(figures_meta$path1[i])$mtime, tz = "UTC"), tz = "UTC")
    figures_meta$mtime2[i] <- as.POSIXct(format(file.info(figures_meta$path2[i])$mtime, tz = "UTC"), tz = "UTC")
  }
  
  .dfpaths <- 
    figures_meta %>% 
    dplyr::select(c("path1", "mtime1", "mtime2", "path2", "compname"))
  
  buildCompare(
    .dfpaths = .dfpaths, 
    .side_by_side = .side_by_side,
    .headings = c(.path_base, .path_compare)
  )
}
