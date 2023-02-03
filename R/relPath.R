`relPath` <-
function( file=dir(),directory=getwd()){
	relDir <- sub("^/","",sub(logRoot(),"",absDir(directory),fixed=TRUE))
	relPath <- file.path(relDir,file)
	relPath[!file.exists(file.path(directory,file))] <- NA
	relPath <- sub("^/","",relPath)
	relPath
}

