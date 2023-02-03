\name{logRoot}
\alias{logRoot}
\title{ Identify the Directory Containing the Review Log }
\description{
  This function searches backward from the current working directory until it finds
  the directory containing the review log.  If no log is found, NULL is returned.
  It is an error if the directory argument does not exist.
}
\usage{
logRoot(directory = getwd())
}
\arguments{
  \item{directory}{ the directory containing the log, or any directory below it }
}
\details{
  This is a recursive function.
}
\value{
character: the full path and name of the directory containing the review log.
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{parentDir}}}
		\item{}{\code{\link{logName}}}
		\item{}{\code{\link{logCreate}}}
		\item{}{\code{\link{logAppend}}}
	}
}
\keyword{manip}

