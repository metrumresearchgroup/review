\name{logWrite}
\alias{logWrite}
\title{ Write a Review Log to Storage in Standard Format }
\description{
  Write a review log, using the same formatting conventions as logRead().
}
\usage{
logWrite(x, file)
}
\arguments{
  \item{x}{ a data.frame, like the output of logQueue() }
  \item{file}{ a file name }
}
\details{
  Calls write.table() with quotes and rownames turned off.  Separator is comma,
  NA's are dots.
  
  Normally the user should not have to call logWrite().  The functions 
  logCreate(), logAppend(), and logRevert() call it.
}
\value{
Used for side effects.
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logRead}}}
		\item{}{\code{\link{logCreate}}}
		\item{}{\code{\link{logAppend}}}
		\item{}{\code{\link{help}}}
	}
}
\keyword{manip}

