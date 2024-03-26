#' @keywords internal
generateFigureComparison <- function(.path) {
  
  .abs_path <- fs::path_abs(.path)
  
  .files <- 
    if (fs::is_dir(.abs_path)) {
      
      list.files(.abs_path, 
                 pattern = "\\.pdf$|\\.png$",
                 full.names = TRUE)
      
    } else {
      
      if (!file.exists(.abs_path)) {
        stop(.path, " does not exist")
      }
      
      .abs_path
    }
  
  if (length(.files) == 0) {
    stop("No figures (PDF or PNG) found in ", .path)
  }
  
  figures_meta <- data.frame()
  
  for (file.i in .files) {
    
    # Local version of the file
    info.i <- list(path1 = file.i, mtime1 = file.info(file.i)$mtime)
    # Convert mtime
    info.i$mtime1 <- as.POSIXct(format(info.i$mtime1, tz = "UTC"), tz = "UTC")
    # SVN info of the file
    svnInfo.i <- svnInfo(file.i)
    
    compareInfo.i <- list(mtime2 = svnInfo.i$datetime)
    
    compareFile.i <-
      svnExport(.file = file.i,
                .revision = as.numeric(svnInfo.i$rev),
                .output_dir = tempdir(), 
                .return_file = TRUE, 
                .quiet = TRUE)
    
    compareInfo.i$path2 <- compareFile.i
    
    compareInfo.i$compname <- basename(file.i)
    
    figures_meta <- rbind(figures_meta,
                          cbind(as.data.frame(info.i), as.data.frame(compareInfo.i)))
    
  }
  figures_meta
}