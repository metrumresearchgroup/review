\name{parentDir}
\alias{parentDir}
\title{ Identify the Parent of a Directory }
\description{
  Generate a fully qualified directory name for the parent of the specified directory.
}
\usage{
parentDir(directory = getwd())
}
\arguments{
  \item{directory}{ the directory whose parent is sought }
}
\details{
  It is an error if the directory does not exist.  If it exists but has no parent,
  NULL is returned. The path includes the volume root.  This function is not 
  vectorized.
}
\value{
character
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{absDir}}}
		\item{}{\code{\link{relPath}}}
		\item{}{\code{\link{logRoot}}}
	}
}
\keyword{manip}

