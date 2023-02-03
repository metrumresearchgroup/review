#`logRevert` <-
#function(directory=logRoot()){
#	old <- logRead(directory)
#	file <- logName(directory)
#	if(exists("log.history",where=1))history <- get("log.history",pos=1)
#	else stop("no log history in this session")
#	if(!file %in% names(history))stop(paste("no history for",file,"in this session"))
#	recent <- length(history[[file]])
#	revert <- history[[file]][[recent]]
#	if(recent==1) history[[file]] <- NULL
#	else history[[file]] <- history[[file]][1:recent-1]
#	if(revert > nrow(old))stop("history inconsistent with log state")
#	if(nrow(old)==0)stop("log has no records")
#	row.names(old) <- nrow(old):1
#	old <- old[as.numeric(row.names(old)) > revert,]
#	logWrite(old,file=file)
#	assign("log.history",history,pos=1)
#	invisible(revert)
#}

