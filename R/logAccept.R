`logAccept` <-
function(
	file=dir(),
	directory=getwd(),
	origin=logOrigin(file,directory),
	reviewer=Sys.info()['user'],
	force=FALSE,
	...
){
	fpath <- logTarget(file=file,directory=directory,force=force)
	opath <- logTarget(file=origin,directory=directory,force=force)
	revf <- sapply(fpath,revision)
	revo <- sapply(opath,revision)
	logAppend(
		new=logQueue(
			file=file,
			directory=directory,
			origin=origin,
			revf=revf,
			revo=revo,
			reviewer=reviewer,
			force=force,
			...
		),
		directory=directory
	)
		
}

