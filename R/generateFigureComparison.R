#' @keywords internal
generateFigureComparison <- function(.path) {
  
  .abs_path <- fs::path_abs(.path)
  
  files_of_interest <-
    try(system(paste0("svn status ", .abs_path, " | grep '^M' | awk '{print $2}'"), intern = TRUE))
  
  if(inherits(files_of_interest, "try-error")){
    stop("Unidentifiable path")
  }
  
  if (length(files_of_interest) == 0) {
    stop("No files from ", .path, " versioned in SVN")
  }
  
  files_of_interest <- files_of_interest[grepl("\\.pdf$|\\.png$", files_of_interest)]
  
  if (length(files_of_interest) == 0) {
    stop("No png or pdf files from ", .path, " versioned in SVN")
  }
  
  figures_meta <- data.frame()
  
  for (file.i in files_of_interest) {
    
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
    
    cli::cli_alert(paste0("Comparing: ", cli::col_blue(fs::path_rel(file.i))))
    
  }
  figures_meta
}
