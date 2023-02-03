\name{revision}
\alias{revision}
\title{ Subversion Revision of a File }
\description{
  Gives the revision of a file that is under subversion control.
}
\usage{
revision(file = logRoot())
}
\arguments{
  \item{file}{ a (path to a) subversioned file}
}
\details{
  If the file is not subversioned, NA is returned. This function is not vectorized,
  so use sapply for multiple files.  The file name will be single-quoted if it
  contains a space and does not start with single or double quote.
}
\value{
numeric
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{author}}}
		\item{}{\code{\link{repoInfo}}}
	}
}
\keyword{manip}

