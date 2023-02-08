#' Create a Review Log
#' 
#' @description 
#' Creates an empty table in the directory specified with name "QClog.csv".
#' 
#' @param directory directory in which to create the log file
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
#' logCreate(directory=getwd())
#' 
#' @author Tim Bergsma
#' 
#' @export
logCreate <- function(directory=getwd()){
	if(file.exists(logName(directory)))stop("log exists")
	logWrite(
		data.frame(
			file=character(0),
			origin=character(0),
			revf=integer(0),
			revo=integer(0),
			reviewer=character(0),
			time=character(0)
		),
		file=logName(directory)
	)
}

