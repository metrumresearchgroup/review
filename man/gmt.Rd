\name{gmt}
\alias{gmt}
\title{Current GMT Time}
\description{
  Display current UTC (GMT).
}
\usage{
gmt()
}
\details{
  Sys.time() returns POSIXlt, which loses time zone when coerced to character.
  This function makes the necessary repair.
}
\value{
character
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logQueue}}}
	}
}
\keyword{manip}

