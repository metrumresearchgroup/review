\name{repoInfo}
\alias{repoInfo}
\title{ Retrieve Repository Information }
\description{
  For a given file, retrieve svn repository information as XML.
}
\usage{
repoInfo(file = logRoot())
}
\arguments{
  \item{file}{a file name }
}
\details{
  Not normally called by the user.
}
\value{
	an XML representation of \code{svn info}
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{revision}}}
		\item{}{\code{\link{author}}}
		\item{}{\code{\link{logRoot}}}
	}
}
\keyword{manip}

