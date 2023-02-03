`logTarget` <-
function(file=dir(),directory=getwd(),force=FALSE){
	target <- file.path(absDir(directory),file)
	missing <- target[!file_test("-f",target)]
	if(length(missing) && !force)stop(paste(length(missing),"nonexistent file(s), e.g.",missing[1]))
	target
}

