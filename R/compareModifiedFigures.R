#' Generate comparison between local (modified) and checked in figures
#' 
#' @description 
#' Allows the user to compare their local version of a figure to the checked
#' in version in SVN. The user can input either a directory of figures or a 
#' singular figure and a HTML page will be generated showing the last version 
#' of the figure checked into SVN next to the local version.
#' 
#' @param .path file path to a directory of figures or a singular figure (pdf or png)
#' 
#' @export
compareModifiedFigures <- function(.path) {
  
  .dfpaths <- getModified(.path, c("png", "pdf"))
  
  .width <- 400
  .height <- round((.width * (7/6)) / 10) *  10
  
  rmd_header <- 
    paste(
      "---",
      paste0("title: \"Output from: ", fs::path_rel(.path), '\"'),
      "output:",
      "  html_document:",
      "    toc: true",
      "    toc_float:",
      "      collapsed: false",
      "    toc_depth: 3",
      "---",
      sep = "\n")
  
  rmd_body <- c()
  
  for (i in 1:nrow(.dfpaths)) {
    
    title.i <- paste0("## ", .dfpaths$compname[i])
    
    graphics.i <- paste0("knitr::include_graphics(c('", .dfpaths$path2[i], "', '", .dfpaths$path1[i], "'))")
    
    left_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:black"><b>Repo</b> (', .dfpaths$mtime2[i], ')</i></div>')
    right_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:blue"><b>Local</b> (', .dfpaths$mtime1[i], ')</i></div>')
    
    caption.i <- paste0('\n', left_caption.i, right_caption.i, '\n')
  
    rmd_body <-
      paste(
        "\n",
        rmd_body,
        title.i,
        caption.i,
        paste0("```{r out.height = ", .height, ", out.width = ", .width, ", echo=FALSE}"),
        graphics.i,
        "```",
        sep = "\n"
      )
  }
  
  rmd_content <- paste(rmd_header, "# Locally modified figures", rmd_body, sep = "\n")
  
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
