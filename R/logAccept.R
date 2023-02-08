#' Approve a File
#' 
#' @description 
#' Indicate acceptance of a file by logging an entry with nonzero version.
#' 
#' @param file one or more files to accept
#' @param directory the parent of the file
#' @param origin the file from which 'file' originates
#' @param reviewer the user issuing the opinion, normally oneself
#' @param force if false (default) nonexistent files cause an error
#' @param ... arguments passed to logQueue
#' 
#' @details 
#' Can be undone with logRevert(). Calls logAppend(), which gives an error if
#' the origin is NA (default) and no precedent exists.
#' 
#' @usage 
#' logAccept(
#' file=dir(),
#' directory=getwd(),
#' origin=logOrigin(file,directory),
#' reviewer=Sys.info()['user'],
#' force=FALSE,
#' ...)
#' 
#' @author Tim Bergsma
#' 
#' @export
logAccept <- function(file=dir(),
                      directory=getwd(),
                      origin=logOrigin(file,directory),
                      reviewer=Sys.info()['user'],
                      force=FALSE,
                      ...){

	fpath <- logTarget(file=file,directory=directory,force=force)
	opath <- logTarget(file=origin,directory=directory,force=force)
	revf <- sapply(fpath,revision)
	revo <- sapply(opath,revision)
	logAppend(
		new=logQueue(
			file=file,
			directory=directory,
			origin=origin,
			revf=revf,
			revo=revo,
			reviewer=reviewer,
			force=force,
			...
		),
		directory=directory
	)
		
}

