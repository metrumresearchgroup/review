#' @importFrom utils write.table
#' @keywords internal
logWrite <- function(x,file) {
  write.table(
	x,
	file=file,
	quote=FALSE,
	sep=",",
	na=".",
	row.names=FALSE
  )
}

