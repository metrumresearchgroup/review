`logPending` <- 
function(directory=getwd()){
	log <- logSummary(directory)
	log[with(log, headf > revf | heado > revo),]
}

