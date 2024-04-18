#' Generate comparison between local (modified) and checked in file(s)
#' 
#' @param .path file or directory path to tables/figures of interest (pdf, png, or tex)
#' 
#' @param .path_compare file or directory path to compare .path to (leave as NULL to compare from svn)
#' 
#' @param .side_by_side Logical. Should outputs be displayed side by side?
#' 
#' @param .display_all Logical. Should all outputs start displayed?
#' 
#' @export
compareModified <- function(.path, .path_compare = NULL, .side_by_side = TRUE, .display_all = FALSE) {
  
  .exts <- c("png", "pdf", "tex")
  
  .dfpaths <-
    
    if (is.null(.path_compare)) { 
      
      getModified(.path, .exts)
      
    } else {
      
      common_files <- intersect(list.files(.path), list.files(.path_compare))
      
      common_files <- common_files[tools::file_ext(files_of_interest) %in% .exts]
      
      if (length(common_files) == 0) {
        stop("No common files in .path and .path_compare of type(s) ", paste(.exts, collapse = ", "))
      }
      
      figures_meta <- 
        data.frame(
          path1 = file.path(fs::path_abs(.path), common_files), 
          path2 = file.path(fs::path_abs(.path_compare), common_files)
        ) %>% 
        dplyr::mutate(compname = basename(path1))
      
      figures_meta$mtime1 <- NA_real_
      figures_meta$mtime2 <- NA_real_
      
      for (i in 1:nrow(figures_meta)) {
        
        cli::cli_alert(paste0("Comparing: ", cli::col_blue(figures_meta$compname[i])))
        
        figures_meta$mtime1[i] <- as.POSIXct(format(file.info(figures_meta$path1[i])$mtime, tz = "UTC"), tz = "UTC")
        figures_meta$mtime2[i] <- as.POSIXct(format(file.info(figures_meta$path2[i])$mtime, tz = "UTC"), tz = "UTC")
      }
      
      figures_meta %>% dplyr::select(path1, mtime1, mtime2, path2, compname)
      
    }
  
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
      paste0("title: '", fs::path_abs(.path), "'"),
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
    
    repo_path.i <- .dfpaths$path2[i]
    local_path.i <- .dfpaths$path1[i]
    
    
    # Unique IDs for HTML elements
    repo_id.i <- paste0("repoContainer", i)
    local_id.i <- paste0("localContainer", i)
    radio_repo_id.i <- paste0("repo", i)
    radio_local_id.i <- paste0("local", i)
    
    graphics.i <- 
      if (!.side_by_side) {
        paste0("
<div>
  <input type='radio' id='", radio_repo_id.i, "' name='toggle", i, "' checked onclick='toggleDisplay(\"", repo_id.i, "\", \"", local_id.i, "\")'>
  <label for='", radio_repo_id.i, "'><i><b>Repo</b></i></label>

  <input type='radio' id='", radio_local_id.i, "' name='toggle", i, "' onclick='toggleDisplay(\"", local_id.i, "\", \"", repo_id.i, "\")'>
  <label for='", radio_local_id.i, "'><i><b style='color:blue'>Local</b></i></label>
</div>

<div id='", repo_id.i, "' style='display:block;'><embed src='", repo_path.i, "' type='application/pdf' width='100%' height='850px' style='border:1px solid #000;'/></div>
<div id='", local_id.i, "' style='display:none;'><embed src='", local_path.i, "' type='application/pdf' width='100%' height='850px' style='border:1px solid #000;'/></div>
<hr>
")
      } else {
        
        left_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:black"><b>Repo</b></i></div>')
        right_caption.i <- paste0('<div style="width: ', .width, 'px; display: inline-block"><i style="color:blue"><b>Local</b></i></div>')
        
        caption.i <- paste0('\n', left_caption.i, right_caption.i, '\n')
        
        paste(
          caption.i,
          paste(
            paste0("```{r out.height = ", .height, ", out.width = ", .width, ", echo=FALSE}"),
            paste0("knitr::include_graphics(c('", repo_path.i, "', '", local_path.i, "'))"),
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
        paste0("<div id=\"collapsibleContent", i, "\" style=\"border: 1px solid #ccc; padding: 10px; display: ", ifelse(.display_all, "block", "none"), ";\">"),
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
