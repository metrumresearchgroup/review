#' List Log Entries that Require Review
#' 
#' @description 
#' Checks the log summary and returns entries where there exists a newer
#'  revision of its file or origin.
#'
#' @usage 
#' logPending(directory = getwd())
#' 
#' @param directory any directory at or below that containing the log
#' 
#' @details 
#' One always wants to review the latest revision of a file, given the latest revision
#' of its origin.  Typically a file is its own origin (e.g. a directly-authored
#' script), in which case it suffices to verify that there is no newer revision.  For 
#' derived objects, the latest revision of the origin is informative, and is 
#' independent of the file revision.  Further review is indicated if either the file,
#' its origin, or both have been revised.  If a file has changed but not its origin,
#' an explanation is needed.  If a file is the same after a change to its origin, confirmation
#' is needed.  It is not unusual for both a file and its origin to change.
#' 
#' @examples 
#' with_demoRepo({
#'   logPending()
#' })
#' 
#' @author Tim Bergsma
#' 
#' @export
logPending <- function(directory=getwd()){
	log <- logSummary(directory)
	log[with(log, headf > revf | heado > revo),]
}

