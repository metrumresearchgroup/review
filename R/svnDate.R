`svnDate` <-
function(file=logRoot()){
	text <- repoInfo(file)
	x <- xmlTreeParse(text,asText=TRUE)
	y <- x$doc$children$info[["entry"]][["commit"]][["date"]][[1]]
	z <- NA
	if(!is.null(y) & length(as.character(y)) >= 6) z <- as.character(y)[[6]]
	z <- sub("T"," ",z)
	z <- sub("\\..*"," GMT",z)
	z
}
