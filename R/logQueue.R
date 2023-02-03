`logQueue` <-
function(
	file=dir(),
	directory=getwd(),
	origin=file,
	revf=0,
	revo=0,
	reviewer="anyone",
	time=gmt(),
	force=FALSE
){
	coerce <- function(arg){
		x <- get(arg)
		if(length(x)==1) x <- rep(x,length(file))
		if(length(x)!=length(file))stop(paste(arg,"should be length 1, or length(file)"))
		x
	}
	confirm <- function(directory,file){
		file <- file[!is.na(file)]
		nonesuch <- file[!file.exists(file.path(directory,file))]
		if(length(nonesuch))warning(
			paste(
				length(nonesuch),
				"nonexistent file(s), e.g.",
				nonesuch[[1]]
			)
		)
	}		
	if(length(directory)!=1)stop("directory should have length one")
	if(!is.character(file))stop("file should be character")
	if(!file_test("-d",directory))stop("directory not found")
	directory <- absDir(directory)
	file <- sub("^/+","",file)
	origin <- sub("^/+","",origin)
	confirm(directory,file)
	confirm(directory,origin)
	revf <- coerce("revf")
	revo <- coerce("revo")
	origin <- coerce("origin")
	reviewer <- coerce("reviewer")
	time <- coerce("time")
	target <- logTarget(file=file,directory=directory,force=force)
	data.frame(
		file=relPath(file=file,directory=directory),
		origin=relPath(file=origin,directory=directory),
		revf=revf,
		revo=revo,
		reviewer=reviewer,
		time=time,
		stringsAsFactors=FALSE
	)
}

