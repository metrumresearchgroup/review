# `logAppendix` <- 
#   function (
#     object = logSummary()[, c('file', 'revf', 'reviewer', 'time')],
#     dir = getwd(), 
#     file = file.path(dir, 'review.tex'),
#     label = 'review', 
#     rowname = NULL,
#     caption = 'Summary of file review: file, last version reviewed (revf), system identifier of reviewer, and time of acceptance.',
#     caption.lot = 'Summary of file review', 
#     where = 'H',
#     show = TRUE,
#     ...
#   ){
#     library(Hmisc)
#     object[] <- lapply(object, latexTranslate)
#     if (nrow(logPending()))warning('see logPending()')
#     out <- latex(
#       object = object, 
#       file = file, 
#       label = label, 
#       rowname = rowname,
#       caption = caption, 
#       caption.lot = caption.lot, 
#       where = 'H',
#       ...
#     )
#     if(show) print(out)
#     invisible(out)
#   }




# `logAssignments` <-
#   function(directory=getwd(),reviewer=Sys.info()[['user']]){
#     log <- logPending(directory)
#     if(!is.null(reviewer)) log <- log[log$reviewer %in% reviewer,]
#     log[,"file"]
#   }

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