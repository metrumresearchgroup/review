`revision` <-
function(file=logRoot()){
	hasSpace <- regexpr(' ',file) > 0
	isQuoted <- regexpr('^["\']',file) > 0
	if(hasSpace & !isQuoted) file <- paste("'",file,"'",sep='')
	text <- repoInfo(file)
	x <- xmlTreeParse(text,asText=TRUE)
	y <- x$doc$children$info[["entry"]][["commit"]]$attributes[["revision"]]
	if(is.null(y))y <- NA
	as.numeric(y)
}

