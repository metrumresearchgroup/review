`logRoot` <-
function(directory=getwd()){
	if(!file_test("-d",directory))stop(paste("nonexistent directory:",directory))
	start <- getwd()
	setwd(directory)
	root <- NULL
	parent <- parentDir(getwd())
	if(file.exists(logName(getwd()))) root <- getwd()
	else if(!is.null(parent)) root <- logRoot(parent)
	setwd(start)
	root
}

