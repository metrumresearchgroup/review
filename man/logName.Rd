\name{logName}
\alias{logName}
\title{ The Full Path and Name of the Log File }
\description{
  Gives a full-path log name for the directory supplied. The default directory
  is logRoot(), in which case the file must already exist.
}
\usage{
logName(directory = logRoot())
}
\arguments{
  \item{directory}{ a directory for which a log name is desired }
}
\details{
  Without arguments, returns full path to log file when current directory is at
  or below the root (directory containing the log).
}
\value{
character
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logRoot}}}
		\item{}{\code{\link{absDir}}}
		\item{}{\code{\link{logCreate}}}
	}
}
\keyword{manip}

