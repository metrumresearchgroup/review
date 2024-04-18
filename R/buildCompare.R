#' Builds a comparative HTML document from specified file paths
#'
#' This internal function creates a detailed comparison report in HTML format
#' from the file paths provided in `.dfpaths`. It supports side-by-side
#' or single view toggling between documents. Currently, it handles TeX files
#' by converting them into PDFs and embedding them into the report.
#'
#' @param .dfpaths A dataframe with columns `compname`, `path1`, and `path2`
#' indicating the name of the comparison set and file paths to compare.
#' @param .side_by_side Logical; if TRUE, display documents side-by-side,
#' otherwise allow toggling between documents with radio buttons.
#' 
#' @param .headings Character. Figure headings.
#'
#' @return The file path of the generated HTML document. This path is returned
#' invisibly and will be automatically opened in a browser if the function
#' is run interactively.
#'
#' @details The function processes each row in `.dfpaths` dataframe and handles
#' each file according to its extension. For `.tex` files, it compiles them to
#' PDF using an internal helper function `tex_to_pdf` which integrates LaTeX
#' packages and wraps the content into a complete LaTeX document before
#' compilation.
#'
#' The function creates a collapsible HTML layout for each comparison set, with
#' optional side-by-side display of documents, or toggling display via radio
#' buttons. Additional JavaScript is embedded for toggling visibility of elements.
#'
#' @keywords internal
buildCompare <- function(.dfpaths, .side_by_side, .headings) {
  
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
      paste0("title: '", fs::path_abs(.dfpaths[["path1"]][1]), "'"),
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
    
    title.i <- paste0("# ", .dfpaths$compname[i])
    
    file_ext.i <- tools::file_ext(.dfpaths$path1[i])
    
    if (file_ext.i == "tex") {
      
      .dfpaths$path1[i] <- tex_to_pdf(.dfpaths$path1[i])
      .dfpaths$path2[i] <- tex_to_pdf(.dfpaths$path2[i])
      
    }
    
    base_path.i <- .dfpaths$path2[i]
    compare_path.i <- .dfpaths$path1[i]
    
    
    # Unique IDs for HTML elements
    base_id.i <- paste0("baseContainer", i)
    compare_id.i <- paste0("compareContainer", i)
    radio_base_id.i <- paste0("base", i)
    radio_compare_id.i <- paste0("compare", i)
    
    graphics.i <- 
      if (!.side_by_side) {
        paste0("
<div>
  <input type='radio' id='", radio_base_id.i, "' name='toggle", i, "' checked onclick='toggleDisplay(\"", base_id.i, "\", \"", compare_id.i, "\")'>
  <label for='", radio_base_id.i, "'><i><b>", .headings[1], "</b></i></label>

  <input type='radio' id='", radio_compare_id.i, "' name='toggle", i, "' onclick='toggleDisplay(\"", compare_id.i, "\", \"", base_id.i, "\")'>
  <label for='", radio_compare_id.i, "'><i><b style='color:blue'>", .headings[2], "</b></i></label>
</div>

<div id='", base_id.i, "' style='display:block;'><embed src='", base_path.i, "' type='application/pdf' width='100%' height='850px' style='border:1px solid #000;'/></div>
<div id='", compare_id.i, "' style='display:none;'><embed src='", compare_path.i, "' type='application/pdf' width='100%' height='850px' style='border:1px solid #000;'/></div>
<hr>
")
      } else {
        
        left_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:black"><b>', .headings[1], '</b></i></div>')
        right_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:blue"><b>', .headings[2], '</b></i></div>')
        
        caption.i <- paste0('\n', left_caption.i, right_caption.i, '\n')
        
        paste(
          caption.i,
          paste(
            paste0("```{r out.height = ", .height, ", out.width = ", .width, ", echo=FALSE}"),
            paste0("knitr::include_graphics(c('", base_path.i, "', '", compare_path.i, "'))"),
            "```",
            sep = "\n"
          ),
          sep = "\n"
        )
      }
    
    rmd_body <-
      paste(
        "\n",
        rmd_body,
        title.i,
        paste0("<button onclick=\"toggleCollapse('collapsibleContent", i, "')\">Show/Hide</button>"),
        paste0("<div id=\"collapsibleContent", i, "\" style=\"border: 1px solid #ccc; padding: 10px; display: ", ifelse(TRUE, "block", "none"), ";\">"),
        graphics.i,
        "</div>",
        "<hr>",
        sep = "\n"
      )
  }
  
  rmd_content <- paste(
    rmd_header, 
    rmd_body, 
    "
<script>
function toggleDisplay(showId, hideId) {
  document.getElementById(showId).style.display = 'block';
  document.getElementById(hideId).style.display = 'none';
}
function toggleCollapse(divId) {
    var content = document.getElementById(divId);
    if (content.style.display === 'none') {
        content.style.display = 'block';
    } else {
        content.style.display = 'none';
    }
}
</script>
<style>
.title{
font-weight: bold;
  font-size: 19px !important;
  color: black;
}
h1 {
  font-weight: bold;
  font-size: 15px;
  color: black;
}

</style>
",
sep = "\n")
  
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
