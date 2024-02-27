#' @noRd
makeFigures <- function(.randnum, .filename) {
  
  # make pdf
  grDevices::pdf(.filename)
  plot(1:.randnum)
  grDevices::dev.off()
  
  # make png
  
  #make jpg
  
  # use this function in demoRepo
  
}