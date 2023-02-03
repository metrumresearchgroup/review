#' Assign Review Tasks
#' 
#' @description 
#' Appends a log with defaults supplied by logQueue().  These are appropriate
#' for assigments.  Default reviewer is set to "anyone".  
#' Default revision is zero, which is diagnostic of an assignment.
#' 
#' @param file a character vector of filenames (paths) relative to directory
#' @param directory the parent of the file
#' @param origin the file from which 'file' originates
#' @param ... arguments passed to logQueue
#' 
#' @details 
#' Since files with revision of zero normally do not exist, no acceptance is 
#' implied by an assigned record. This function may be used to specify a reviewer 
#' for certain files.  Default reviewer "anyone" simply declares that an entity
#' should be reviewed. "Origin" files may also be specified, indicating that a 
#' file originates from (e.g. is created by) some other file.  The function 
#' logPending() identifies files needing (further) review.  A file's origin
#' is itself, by default. Currently, logAssign() calls logAppend(), which allows
#' NA for origin (defaults to precedent or to file itself).
#' 
#' @usage 
#' logAssign(
#'  file = dir(), 
#'  directory = getwd(), 
#'  origin = file,
#'  ...)
#' 
#' @author Tim Bergsma
#' 
#' @export
logAssign <- function(file=dir(),
                      directory=getwd(),
                      origin=file,
                      ...) {
  logAppend(
	new=logQueue(file=file,directory=directory,origin=origin,...),
	directory=logRoot(directory)
  )
}

