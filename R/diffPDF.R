#' Create a GIF of original vs modified PDF
#' 
#' @description 
#' Compares the local version of a PDF with the most recent version checked into
#' SVN. A GIF will be created to help the user compare the original file to the 
#' modified one.
#'
#' @param .file file path from working directory
#' @param .fps frames per second, increase for quicker switching between images
#' 
#' @examples 
#' \dontrun{
#' diffPDF(.file = "example-1.pdf", .fps = 1)
#' }
#' 
#' @export
diffPDF <- function(.file, .fps = 1) {
  
  # Remove 20-23 and make the outputs of this the inputs for the function
  get_current_prev <-
    getPreviousCurrent(.file,
                       .previous_revision = svnProjInfo()[["rev"]],
                       .current_revision = NULL)
  
  paths <- list(previous = get_current_prev$.previous_revision_temp_file,
                current = get_current_prev$.current_revision_temp_file)
  
  img_list <- lapply(paths, magick::image_read)
  
  if(length(img_list[["previous"]]) != length(img_list[["current"]])){
    stop("must have same num pages")
  }
  
  for(i in 1:length(img_list[["previous"]])){
    
    this_page.i <-
      magick::image_join(
        magick::image_annotate(img_list[["previous"]][i], text = paste0("Page ", i, " (original)"), gravity = "south", color = "red"),
        magick::image_annotate(img_list[["current"]][i], text = paste0("Page ", i, " (modified)"), gravity = "south", color = "red")
      )
    
    img_list_all_pg <-
      if(i == 1){
        this_page.i
      } else {
        magick::image_join(img_list_all_pg, this_page.i)
      }
  }
  
  img_animated <- magick::image_animate(image = img_list_all_pg, fps = .fps)
  
  
  img_animated
}