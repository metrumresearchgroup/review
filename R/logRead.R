`logRead` <-
function(directory=getwd()){
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
	if('revision' %in% names(log))stop(paste(
		'please install review 1.5 to support this log',
		logName(logRoot(directory))
	))
	if(with(log,any(file==origin & revf!=revo)))warning('matching file/origin should have matching revisions')
	log
}

