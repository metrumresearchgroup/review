\name{review-package}
\alias{review-package}
\alias{review}
\docType{package}
\title{
 Manage Review Logs
}
\description{
Functions for managing logs of reviews of subversioned files (http://subversion.apache.org/).
}
\details{
\tabular{ll}{
Package: \tab review\cr
Type: \tab Package\cr
Version: \tab 2.5\cr
Date: \tab 2013-04-18\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
These functions are rather flexible, but convenient defaults are chosen.  Easiest
usage is described here.

\bold{Making a Log}
\enumerate{
\item In R, navigate to the check-out directory with \code{setwd()}.
\item Type \code{logCreate()}.
\item Be sure to add your log file and check it in with svn tools.
}
\bold{Making Assignments}
\enumerate{
\item In R, navigate to the check-out directory with \code{setwd()}.
\item Try \code{dir(recursive=TRUE)} to get a list of possible assignments.  
customize as necessary.
\item To assign all files to "anyone", type \code{logAssign(dir(recursive=TRUE))}.
\item To assign just the files in a directory, navigate there and type \code{logAssign()}.
\item To review the result, type \code{logRead()}.
\item To undo the result, type \code{logRevert()}.
\item Check in your changes using svn tools.
}
\bold{Reviewing Files}
\enumerate{
\item Update your checkout with svn tools.
\item Naviate to the directory containing the log, or below.
\item View your assignments with \code{logAssignments()} or \code{logAssignments(reviewer = 'anyone')}.
\item Capture the name of a particular file, e.g. \code{f <- logAssignments()[[1]]}.
\item To approve the file, say \code{logAccept(f)}.
\item Check in your changes using svn tools.
}
\bold{Summarizing}
\enumerate{
\item Navigate to the directory containing the log, or below.
\item Type \code{logSummary()}.
\item If \code{logPending()} is empty, all assigned reviews are complete.
}
\bold{Shortcut}
\enumerate{
\item Technically, it is not necessary that a file ever be assigned.  The first acceptance of an
	existing file suffices (origin must be specified, however). The only two essential
	functions are \code{logCreate()} and \code{logAccept()}.
}
}
\author{
Tim Bergsma

Maintainer: Tim Bergsma <timb@metrumrg.com>
}
\references{
 Development supported by Metrum Institute:  http://metruminstitute.org.
}
\keyword{ package }
\seealso{
\itemize{
	\item \code{\link{absDir}}
	\item \code{\link{parentDir}}
	\item \code{\link{logName}}
	\item \code{\link{logCreate}}
	\item \code{\link{logRoot}}
	\item \code{\link{repoInfo}}
	\item \code{\link{revision}}
	\item \code{\link{author}}
	\item \code{\link{svnDate}}
	\item \code{\link{gmt}}
	\item \code{\link{logRead}}
	\item \code{\link{logWrite}}
	\item \code{\link{relPath}}
	\item \code{\link{logTarget}}
	\item \code{\link{logQueue}}
	\item \code{\link{logAppend}}
	\item \code{\link{logAssign}}
	\item \code{\link{logAccept}}
	\item \code{\link{logSummary}}
	\item \code{\link{logPending}}
	\item \code{\link{logAssignments}}
	\item \code{\link{logAppendix}}
}
}
\keyword{manip}

