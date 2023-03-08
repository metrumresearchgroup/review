#' @keywords internal
pathFromLogRoot <- function(.path){
  
  cur_dir <- getwd()
  on.exit(setwd(cur_dir))
  
  log_root <- logRoot()
  setwd(log_root)
  
  full_path <- as.character(fs::path_abs(.path))
  full_here <- as.character(fs::path_abs(log_root))
  
  .ans <- gsub(paste0(full_here, "/"), "", full_path, fixed = TRUE)
  
  .ans
}