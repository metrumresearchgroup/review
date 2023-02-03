`parentDir` <-
function(directory=getwd()){
	if(!file_test("-d",directory))stop(paste("nonexistent directory:",directory))
	directory <- absDir(directory)
	start <- getwd()
	setwd(directory)
	parent <- NULL
	if(file_test("-d","..")){
		setwd("..")
		if(getwd()!=absDir(directory)) parent <- getwd()
	}
	setwd(start)
	parent
}

