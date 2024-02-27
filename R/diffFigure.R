#' Create a GIF of original vs modified figure
#'
#' @description
#' Compares the local version of a figure with the most recent version checked into
#' SVN. A GIF will be created to help the user compare the original file to the
#' modified one.
#'
#' @param .file1 file path from working directory
#' @param .file2 file path from working directory
#' @param .fps frames per second, increase for quicker switching between images
#'
#' @examples
#' \dontrun{
#' diffFigure(
#'   .file1 = "example-1.pdf",
#'   .file2 = "example-2.pdf"
#'   )
#' }
#'
#' @export
diffFigure <- function(.file1, .file2 = NULL, .fps = 2) {
  
  if (!file.exists(.file1)) {
    paste0(.file1, " does not exist")
  }
  
  if(is.null(.file2)) {
    # Compare local (modified) to last checked in version
    
    get_current_prev <-
      getPreviousCurrent(.file1,
                         .previous_revision = svnProjInfo()[["rev"]],
                         .current_revision = NULL)
    
    .file2 <- get_current_prev$.previous_revision_temp_file
  }
  
  if (!file.exists(.file2)) {
    paste0(.file2, " does not exist")
  }
  
  paths <- list(previous = .file1, current = .file2)
  
  img_list <- lapply(paths, magick::image_read)
  
  len_prev <- length(img_list[["previous"]])
  len_cur <- length(img_list[["current"]])
  
  if (len_prev != len_cur) {
    stop(".file1 and .file2 must have same number of pages", call. = FALSE)
  }
  
  for (i in 1:len_prev) {
    
    this_page.i <-
      magick::image_join(
        magick::image_annotate(
          img_list[["previous"]][i],
          text = paste0("Page ", i, " (original)"),
          #gravity = "southeast",
          #location = "+50+20", 
          color = "red"
        ),
        magick::image_annotate(
          img_list[["current"]][i],
          text = paste0("Page ", i, " (modified)"),
          #gravity = "southeast",
          #location = "+50+20", 
          color = "blue"
        )
      )
    
    img_list_all_pg <-
      if (i == 1) {
        this_page.i
      } else {
        magick::image_join(img_list_all_pg, this_page.i)
      }
  }
  
  magick::image_animate(image = img_list_all_pg, fps = .fps)
}
