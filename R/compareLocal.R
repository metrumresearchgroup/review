#' Generate comparison between sets of local file(s)
#' 
#' @param .path file or directory path to tables/figures of interest (pdf, png, or tex)
#' 
#' @param .path_compare file or directory path to compare .path to
#' 
#' @param .side_by_side Logical. Should outputs be displayed side by side?
#' 
#' @export
compareLocal <- function(.path, .path_compare, .side_by_side = TRUE) {
  
  .exts <- c("png", "pdf", "tex")
  
  .is_dir <- fs::is_dir(.path)
  
  common_files <- 
    
    if (.is_dir) {
      
      intersect(list.files(.path), list.files(.path_compare))
      
    } else {
      
      intersect(basename(.path), basename(.path_compare))
      
    }
  
  common_files <- common_files[tools::file_ext(common_files) %in% .exts]
  
  if (length(common_files) == 0) {
    stop("No common files in .path and .path_compare of type(s) ", paste(.exts, collapse = ", "))
  }
  
  if (.is_dir) {
    
    .path1 <- file.path(fs::path_abs(.path), common_files)
    .path2 <- file.path(fs::path_abs(.path_compare), common_files)
    
  } else{
    
    .path1 <- fs::path_abs(.path)
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
  
  buildCompare(.dfpaths = .dfpaths, .side_by_side = .side_by_side)
}
