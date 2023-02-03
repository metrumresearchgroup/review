`logAssignments` <-
function(directory=getwd(),reviewer=Sys.info()[['user']]){
	log <- logPending(directory)
	if(!is.null(reviewer)) log <- log[log$reviewer %in% reviewer,]
	log[,"file"]
}

