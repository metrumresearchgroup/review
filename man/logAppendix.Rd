\name{logAppendix}
\alias{logAppendix}
\title{Convert a Log Summary to a Report-ready Table}
\description{
  Converts the output of \code{logSummary} to a Latex table, suitable 
  for inclusion in a report, e.g. as an appendix.  Gives a warning if logPending()
  has rows.
}
\usage{
logAppendix(
	object=logSummary()[,c('file','revf','reviewer','time')],
	dir=getwd(),
	file=file.path(dir,'review.tex'),
	label='review',
	rowname=NULL,
	caption='Summary of file review: file, last version reviewed (revf), system identifier of reviewer, and time of acceptance.',
	caption.lot='Summary of file review',
	where='H',
	show=TRUE,
	...
)
}
\arguments{
  \item{object}{object to be plotted}
  \item{dir}{a directory used to build the default file path}
  \item{file}{a file path for the output}
  \item{label}{a character string used for the \code{tex} label}
  \item{rowname}{default NULL suppresses rownames}
  \item{caption}{table caption}
  \item{caption.lot}{caption variant for use in list of tables}
  \item{where}{default H ("here") places table exactly where specified}
  \item{show}{default TRUE invokes the dvi viewer}
  \item{\dots}{passed to \code{latex}}
}
\details{
  This function calls \code{latex} in package Hmisc.  All arguments except
  \code{dir} are passed to \code{latex}.   \code{dir} is just a 
  convenience argument to place the output in other than the current
  working directory (using the default file name).  It is ignored
  if \code{file} is supplied.
}
\value{
used for side effects
}
\references{ http://metruminstitute.org }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{logSummary}}}
	}
}
\keyword{manip}

