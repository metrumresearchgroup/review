`logCreate` <-
function(directory=getwd()){
	if(file.exists(logName(directory)))stop("log exists")
	logWrite(
		data.frame(
			file=character(0),
			origin=character(0),
			revf=integer(0),
			revo=integer(0),
			reviewer=character(0),
			time=character(0)
		),
		file=logName(directory)
	)
}

