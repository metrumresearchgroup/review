\name{absDir}
\alias{absDir}
\title{ Absolute Directory }
\description{
  Return the full path of a directory.
}
\usage{
absDir(directory)
}
\arguments{
  \item{directory}{an absolute or relative path of a directory.}
}
\details{
  Relative paths include subdirectories, "..",".", "~","~/", etc. absDir() should
  return the complete path, back to the volume root.
}
\value{
character
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{relPath}}}
	}
}
\keyword{manip}

