.onAttach <- function(libname,pkgname){
	packageStartupMessage(paste('review',utils::packageDescription('review',fields='Version')))
}
