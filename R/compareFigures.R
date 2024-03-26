#' Generate diff between figures
#' 
#' @description 
#' Allows the user to compare the differences between figures. The user can
#' input either a directory of figures or a singular figure and a HTML page
#' will be generated showing the last version of the figure checked into SVN 
#' next to the local version.
#' 
#' @param .path file path to a figure (pdf or png) or a directory of figures
#' 
#' @export
compareFigures <- function(.path) {
  
  .dfpaths <- generateFigureComparison(.path)
  
  rmd_content <- 
    paste(
      "---",
      paste0("title: \"Figure comparison: ", .path, '\"'),
      "date: '`r Sys.time()`'",
      "output:",
      "  html_document:",
      "    toc: true",
      "    toc_float:",
      "      collapsed: true",
      "---",
      sep = "\n")
  
  .dfpaths$graphics <- paste0("knitr::include_graphics(c('", .dfpaths$path2, "', '", .dfpaths$path1, "'))")
  
  for (row.i in 1:nrow(.dfpaths)) {
    
    rmd_content <- paste(
      "\n",
      rmd_content,
      paste0("# ", .dfpaths$compname[row.i]),
      "```{r out.height = 360, echo=FALSE, fig.cap=''}",
      .dfpaths$graphics[row.i],
      "```",
      paste0("*Left last modified: ",
             .dfpaths$mtime2[row.i], 
             "*<br>*Right last modified: ",
             .dfpaths$mtime1[row.i], "*\n"),
      sep = "\n")
  }
  
  outputFileName = tempfile("compare-figures", fileext = ".Rmd")
  
  writeLines(rmd_content, con = outputFileName)
  
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
  
  return(invisible(temp_out))
}