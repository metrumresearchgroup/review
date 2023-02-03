`logAssign` <-
function(file=dir(),directory=getwd(),origin=file,...)logAppend(
	new=logQueue(file=file,directory=directory,origin=origin,...),
	directory=logRoot(directory)
)

