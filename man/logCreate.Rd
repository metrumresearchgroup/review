\name{logCreate}
\alias{logCreate}
\title{ Create a Review Log }
\description{
  Creates an empty table in the directory specified with name "QClog.csv".
}
\usage{
logCreate(directory = getwd())
}
\arguments{
  \item{directory}{directory in which to create the log file}
}
\details{
  Columns are as follows.  
  \describe{
  	\item{file}{the path and name of the accepted file, relative to the specified directory, and not beginning with "/"}
	\item{origin}{identical to \code{file} or the name of some other file responsible for this content}
	\item{revf}{the reviewed revision of \code{file}}
	\item{revo}{the review-time version of \code{file}'s origin}
	\item{reviewer}{the \R user name of the reviewer of \code{file}}
	\item{time}{date and time (GMT) of acceptance of \code{file}}
  }
}
\value{
used for side effects.
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item \code{\link{logName}}
		\item \code{\link{logAssign}}
		\item \code{\link{logAccept}}
	}
}
\keyword{manip}

