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
  
  rmd_header <- 
    paste(
      "---",
      paste0("title: \"Figure comparison: ", fs::path_rel(.path), '\"'),
      "date: '`r Sys.time()`'",
      "output:",
      "  html_document:",
      "    toc: true",
      "    toc_float:",
      "      collapsed: true",
      "---",
      sep = "\n")
  
  rmd_body <- c()
  
  for (i in 1:nrow(.dfpaths)) {
    
    title.i <- paste0("# ", .dfpaths$compname[i])
    
    graphics.i <- paste0("knitr::include_graphics(c('", .dfpaths$path2[i], "', '", .dfpaths$path1[i], "'))")
    
    caption.i <- paste0('\n<div style="display: flex;"><div style="width: 50%; text-align: left;"><i style="color:blue">Repo (left) last modified: 2024-03-28 15:23:17</i></div><div style="width: 50%; text-align: right;"><i>Local (right) last modified: 2024-03-28 15:23:18</i></div></div>\n')
    
    # caption.i <- paste0("\n<i style = 'color:blue'>Repo (left) last modified: ",
    #                     .dfpaths$mtime2[i],
    #                     "</i><br><i>Local (right) last modified: ",
    #                     .dfpaths$mtime1[i], "</i>\n")
    
    rmd_body <-
      paste(
        "\n",
        rmd_body,
        
        title.i,
        "```{r out.height = 360, echo=FALSE}",
        graphics.i,
        "```",
        caption.i,
        sep = "\n"
      )
  }
  
  rmd_content <- paste(rmd_header, rmd_body, sep = "\n")
  
  rmd_file = tempfile("compare-figures", fileext = ".Rmd")
  
  writeLines(rmd_content, con = rmd_file)
  
  html_file <- tempfile(fileext = ".html")
  
  rmarkdown::render(
    rmd_file,
    output_file = html_file,
    envir = new.env(),
    quiet = TRUE
  )
  
  if (interactive()) {
    utils::browseURL(html_file)
  }
  
  return(invisible(html_file))
}
