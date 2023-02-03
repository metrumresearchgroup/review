\name{logQueue}
\alias{logQueue}
\title{ Prepare Records for Review Log }
\description{
  Create one or more records with the proper format for appending to a review log.
}
\usage{
logQueue(
	file=dir(),
	directory=getwd(),
	origin=file,
	revf=0,
	revo=0,
	reviewer="anyone",
	time=gmt(),
	force=FALSE
)
}
\arguments{
  \item{file}{ one or more files for which to create records}
  \item{directory}{ the parent of file }
  \item{origin}{ file(s) from which 'file' originates}
  \item{revf}{ the svn revision of the file being reviewed }
  \item{revo}{ the svn revision of the origin of file}
  \item{reviewer}{  the user issuing the opinion, normally oneself }
  \item{time}{ character string: date and time of acceptance, using GMT (UTC) }
  \item{force}{if false (the default) nonexistent files cause an error  }
}
\details{
  The required format for a review log is described in \code{\link{logCreate}}. 
  Functions like logAssign() and logAccept() send output to the log.  The function
  logQueue() creates a data.frame that can be viewed, assigned, and manipulated prior
  to appending the log.  You may want to use logQueue() in conjunction with logAppend().
  That is exactly what logAssign() and logAccept() do.
  
  Note that time should be character, each element ending with "GMT".
}
\value{
data.frame, with a format much like QClog.csv
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logCreate}}}
		\item{}{\code{\link{logAssign}}}
		\item{}{\code{\link{logAccept}}}
		\item{}{\code{\link{logAppend}}}
	}
}

\keyword{manip}

