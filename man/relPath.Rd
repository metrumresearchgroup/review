\name{relPath}
\alias{relPath}
\title{ Create File Paths Relative to the Log Root }
\description{
  For the files specified by the arguments, create file paths relative to
  the directory containing the review log.
}
\usage{
relPath(file = dir(), directory = getwd())
}
\arguments{
  \item{file}{one or more files for which to create paths}
  \item{directory}{the parent directory of file}
}
\details{
  The elements of 'file' must be encoded relative to 'directory'.
}
\value{
character vector
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{parentDir}}}
		\item{}{\code{\link{absDir}}}
		\item{}{\code{\link{logRoot}}}
		\item{}{\code{\link{help}}}
	}
}

\keyword{manip}

