\name{author}
\alias{author}
\title{ Subversion Author of a File }
\description{
  Gives the author of a file that is under subversion control.
}
\usage{
author(file = logRoot())
}
\arguments{
  \item{file}{ a (path to a) subversioned file}
}
\details{
  If the file is not subversioned, NA is returned. This function is not vectorized,
  so use sapply for multiple files.
}
\value{
character
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{revision}}}
		\item{}{\code{\link{repoInfo}}}
	}
}
\keyword{manip}

