`logSummary` <-
function(directory=getwd()){
	log <- logRead(directory)
	if(any(is.na(log$file)))stop("missing file names in log")
	log <- log[order(log$file, log$revf, log$time),]
	log <- log[!duplicated(log$file,fromLast=TRUE),]
	cols <- c('file','origin','revf','headf','revo','heado','reviewer','time')
	if(!nrow(log))return(cbind(log,data.frame(headf=numeric(0),heado=numeric(0))))[,cols]
	absFile <- logTarget(file=log$file,directory=logRoot(directory),force=TRUE)
	absOrigin <- logTarget(file=log$origin,directory=logRoot(directory),force=TRUE)
	log$headf <- sapply(absFile,revision)
	log$heado <- sapply(absOrigin,revision)
	log <- log[,cols]
	class(log) <- c('logSummary','data.frame')
	log
}
print.logSummary <- function(x,...){
	x$origin[x$origin==x$file] <- ''
	x$revo[x$origin=='' & x$revo == x$revf] <- ''
	x$heado[x$origin=='' & x$heado == x$headf] <- ''
	if(all(x$origin==''))x$origin <- NULL
	if(all(x$revo==''))x$revo <- NULL
	if(all(x$heado==''))x$heado <- NULL
	NextMethod()
}
