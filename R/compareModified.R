#' Generate comparison between local (modified) and checked in file(s)
#' 
#' @param .path file or directory path to tables/figures of interest (pdf, png, or tex)
#' 
#' @export
compareModified <- function(.path) {
  
  .dfpaths <- getModified(.path, c("png", "pdf", "tex"))
  
  tex_to_pdf <- function(.tex_path){
    
    temp_tex.i <- tempfile(fileext = ".tex")
    tex_content.i <- readLines(.tex_path)
    
    complete_tex_document.i <- c(
      "\\documentclass{article}",
      "\\usepackage{threeparttable}",
      "\\usepackage{longtable}",
      "\\usepackage{array}",
      "\\begin{document}",
      tex_content.i,
      "\\end{document}"
    )
    
    writeLines(complete_tex_document.i, temp_tex.i)
    temp_pdf.i <- tinytex::pdflatex(temp_tex.i)
    temp_pdf.i
  }
  
  .width <- 400
  .height <- round((.width * (7/6)) / 10) *  10
  
  rmd_header <- 
    paste(
      "---",
      paste0("title: \"Path: ", fs::path_rel(.path), '\"'),
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
    
    file_ext.i <- tools::file_ext(.dfpaths$path1[i])
    
    if (file_ext.i == "tex") {
      
      .dfpaths$path1[i] <- tex_to_pdf(.dfpaths$path1[i])
      .dfpaths$path2[i] <- tex_to_pdf(.dfpaths$path2[i])
      
    }
    
    graphics.i <- 
      
      paste(
        paste0("```{r out.height = ", .height, ", out.width = ", .width, ", echo=FALSE}"),
        paste0("knitr::include_graphics(c('", .dfpaths$path2[i], "', '", .dfpaths$path1[i], "'))"),
        "```",
        sep = "\n"
      )
    
    left_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:black"><b>Repo</b> (', .dfpaths$mtime2[i], ')</i></div>')
    right_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:blue"><b>Local</b> (', .dfpaths$mtime1[i], ')</i></div>')
    
    caption.i <- paste0('\n', left_caption.i, right_caption.i, '\n')
    
    rmd_body <-
      paste(
        "\n",
        rmd_body,
        title.i,
        caption.i,
        graphics.i,
        sep = "\n"
      )
  }
  
  rmd_content <- paste(rmd_header, "# Locally modified files", rmd_body, sep = "\n")
  
  rmd_file <- tempfile("compare-figures", fileext = ".Rmd")
  
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
