\name{logAssignments}
\alias{logAssignments}
\title{ Detect Files to be Reviewed }
\description{
  Gives a vector of the pending files assigned to reviewer (normally oneself).
}
\usage{
logAssignments(directory = getwd(), reviewer = Sys.info()[["user"]])
}
\arguments{
  \item{directory}{ the directory containing the log, or any directory below it}
  \item{reviewer}{a user name on which to filter results}
}
\details{
  "Pending" has the meaning described for logPending().  "Assigned" has the meaning
  described for logAssign().  A character vector is returned, from which elements
  can be selected for use as arguments to logAccept().
}
\value{
character
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logAssign}}}
		\item{}{\code{\link{logPending}}}
		\item{}{\code{\link{logAccept}}}
	}
}
\keyword{manip}

