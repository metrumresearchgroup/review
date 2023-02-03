\name{logTarget}
\alias{logTarget}
\title{ Generate Fully Qualified Filenames }
\description{
  Prepend the absolute file path to each file name, and test for existence.
}
\usage{
logTarget(file = dir(),directory = getwd(), force = FALSE)
}
\arguments{
  \item{file}{one or more files for which to create targets}
  \item{directory}{the parent of file}
  \item{force}{if false (the default) nonexistent files cause an error}
}
\details{
 This function is vectorized.
}
\value{
 character vector of file names
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{absDir}}}
		\item{}{\code{\link{logQueue}}}
	}
}
\keyword{manip}

