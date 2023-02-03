\name{logAccept}
\alias{logAccept}
\title{ Approve a File }
\description{
  Indicate acceptance of a file by logging an entry with nonzero version.
}
\usage{
logAccept(
	file=dir(),
	directory=getwd(),
	origin=logOrigin(file,directory),
	reviewer=Sys.info()['user'],
	force=FALSE,
	...
)
}
\arguments{
  \item{file}{ one or more files to accept }
  \item{directory}{ the parent of file }
  \item{origin}{  the file from which 'file' originates }
  \item{reviewer}{ the user issuing the opinion, normally oneself }
  \item{force}{ if false (default) nonexistent files cause an error }
  \item{\dots}{ arguments passed to logQueue }
}
\details{
  Can be undone with logRevert(). Calls logAppend(), which gives an error if
  origin is NA (default) and no precedent exists.
}
\value{
(invisible) number of records created.
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logAssign}}}
		\item{}{\code{\link{logQueue}}}
		\item{}{\code{\link{logRoot}}}
	}
}
\keyword{manip}

