#' @importFrom utils file_test
#' @importFrom utils read.table
#' @keywords internal
logRead <- function(directory=getwd()){
	if(is.null(directory))stop("directory is null: log may not exist")
	if(!file_test("-d",directory))stop(paste("nonexistent directory:",directory))
	root <- logRoot(directory)
	if(is.null(root))stop(paste(absDir(directory),"does not have a log root"))
	log <- read.table(
		logName(logRoot(directory)),
		header=TRUE,
		sep=",",
		as.is=TRUE,
		na.strings=".",
		strip.white=TRUE
	)
	
	if (nrow(log) > 0) {
  	for (i in 1:nrow(log)) {
  	  log$file[i] <- pathFromLogRoot(log$file[i])
  	  log$origin[i] <- pathFromLogRoot(log$origin[i])
  	}
  	unique_rows <- !duplicated(log)
  	log <- log[unique_rows,]
  	rm(unique_rows)
	}
	
	if('revision' %in% names(log))stop(paste(
		'please install review 1.5 to support this log',
		logName(logRoot(directory))
	))
	if(with(log,any(file==origin & revf!=revo)))warning('matching file/origin should have matching revisions')
	log
}

