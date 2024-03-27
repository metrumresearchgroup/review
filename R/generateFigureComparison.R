#' @keywords internal
generateFigureComparison <- function(.path, .svnmodify = FALSE) {
  
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
  
  svnstatus <- svnCommand("status")
  
  svnentries <- svnstatus$target
  
  svndata <- data.frame()
  
  for (i in 1:length(svnentries)) {
    
    svnfile <- svnentries[[i]]
    
    if (!"path" %in% names(svnfile)) {
      
      svnfile.i = as.character(svnfile$.attrs)
      svnstatus.i = svnfile$`wc-status`$.attrs[["item"]]
      
      svndata <-
        rbind(
          svndata,
          data.frame(file = svnfile.i, status = svnstatus.i)
        )
    }
  }
  
  svndata2 <- svndata[svndata$status != "unversioned",]
  svndata2$file <- fs::path_abs(svndata2$file)
  
  files_of_interest <- .files[.files %in% svndata2$file]
  
  if (length(files_of_interest) == 0) {
    stop("No files from ", .path, " versioned in SVN")
  }
  
  if (.svnmodify) {
    svn_modified_files <- svndata2[svndata2$status == "modified",]
    files_of_interest <- files_of_interest[files_of_interest %in% svn_modified_files$file]
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
    
  }
  figures_meta
}