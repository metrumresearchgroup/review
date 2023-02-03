\name{logAppend}
\alias{logAppend}
\alias{logOrigin}
\title{ Append a Review Log }
\description{
  Add records to a review log.
}
\usage{
logAppend(new,directory = logRoot(),  ...)
logOrigin(file,directory = logRoot(), ...)
}
\arguments{
  \item{new}{ a data.frame of new records to append, as created with logQueue(). }
  \item{directory}{ the directory containing the log, or any directory below it }
  \item{file}{ vector of file names. }
  \item{\dots}{ arguments passed to logQueue(). }
}
\details{
  Generally these functions are not called by the user.  Check "see also" for preferred interface.
}
\value{
logAppend: (invisible) the number of rows added.  logOrigin: the precedents for 
file origin, if any, or file itself where precedent not available.
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logAssign}}}
		\item{}{\code{\link{logQueue}}}
		\item{}{\code{\link{logAccept}}}
	}
}
\keyword{manip}

