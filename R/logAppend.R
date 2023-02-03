`logAppend` <-
function(new,directory=logRoot(),...){
	old <- logRead(directory)
	mix <- rbind(old,new)
	if(with(mix,any(file==origin & revf!=revo)))stop('matching file/origin must have matching revisions')
	file <- logName(logRoot(directory))
	if(!file.exists(file))stop(paste("can't find",file))
	logWrite(mix,file=file)
	#history <- list()
	#if(exists("log.history",where=1)) history <- get("log.history",pos=1)
	newrows <- nrow(new)
	#if(is.null(history[[file]])) history[[file]] <- newrows
	#else history[[file]] <- append(history[[file]],newrows)
	#if(newrows > 0) assign("log.history",history,pos=1)
	invisible(newrows)
}
logOrigin <- function(file, directory=logRoot(),...){
	old <- logRead(directory)
	precedent <- old$origin
	names(precedent) <- old$file
	precedent <- precedent[order(old$file,old$time)]
	precedent <- precedent[!is.na(precedent) & !duplicated(names(precedent),fromLast=TRUE)]
	precedent <- precedent[file]
	precedent[is.na(precedent)] <- file[is.na(precedent)]
	as.character(precedent)
}

