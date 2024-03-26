#' Generate diff between figures
#' 
#' @description 
#' Allows the user to compare the differences between two figures or different
#' versions of the same figure. If file paths are provided to both the `figure1` 
#' and `figure2` argument, a HTML page will be generated showing both side by
#' side. 
#' 
#' If only the `figure1` argument is provided, a HTML page will be generated
#' showing the last version of the figure checked into SVN next to the
#' local version.
#' 
#' @param figure1 file path to figure 1
#' @param figure2 file path to figure 2 (optional)
#'
#' @export
diffFigures <- function(figure1, figure2 = NULL) {
  
  if (!file.exists(figure1)) {
    stop(figure1, " does not exist")
  }
  
  figure1Info <- list(path1 = figure1, mtime1 = file.info(figure1)$mtime)
  
  figure1Info$mtime1 <- as.POSIXct(format(figure1Info$mtime1, tz = "UTC"), tz = "UTC")
  
  if (is.null(figure2)) {
    
    figure2svnInfo <- svnInfo(figure1)
    
    figure2Info <- list(mtime2 = figure2svnInfo$datetime)
    
    figure2 <-
      svnExport(.file = figure1,
                .revision = as.numeric(figure2svnInfo$rev),
                .output_dir = tempdir(), 
                .return_file = TRUE, 
                .quiet = TRUE)
    
    figure2Info$path2 <- figure2
    
    figure2Info$compname <- paste0(basename(figure1), ": repo vs local") 
    
  } else {
    figure2Info <- list(path2 = figure2, mtime2 = file.info(figure2)$mtime)
    figure2Info$mtime2 <- as.POSIXct(format(figure2Info$mtime2, tz = "UTC"), tz = "UTC")
    figure2Info$compname <- paste0(basename(figure2), " vs ", basename(figure1))
  }
  
  if (!file.exists(figure2)) {
    stop(figure2, " does not exist")
  }
  
  dfpaths <- cbind(as.data.frame(figure1Info), as.data.frame(figure2Info))
  
  content <- 
    paste(
      "---",
      "title: \"Diff Figures\"",
      "date: '`r Sys.time()`'",
      "output:",
      "  html_document:",
      "    toc: true",
      "    toc_float:",
      "      collapsed: true",
      "---",
      sep = "\n")
  
  dfpaths$graphics <- paste0("knitr::include_graphics(c('", dfpaths$path2, "', '", dfpaths$path1, "'))")
  
  for (row.i in 1:nrow(dfpaths)) {
    
    content <- paste(
      content,
      paste0("# ", dfpaths$compname[row.i]),
      "```{r out.height = 360, echo=FALSE, fig.cap=''}",
      dfpaths$graphics[row.i],
      paste0("print('Left: ", basename(dfpaths$path2), ", last modified: ", dfpaths$mtime2, "')"),
      paste0("print('Right: ", basename(dfpaths$path1), ", last modified: ", dfpaths$mtime1, "')"),
      "```",
      sep = "\n")
  }
  
  outputFileName = tempfile("diff-figure", fileext = ".Rmd")
  
  writeLines(content, con = outputFileName)
  
  # output ------------------------------------------------------------------
  temp_out <- tempfile(fileext = ".html")
  
  rmarkdown::render(
    outputFileName,
    output_file = temp_out,
    envir = new.env(),
    quiet = TRUE
  )
  
  if (interactive()) {
    utils::browseURL(temp_out)
  }
  
  return(invisible(dfpaths))
}