`repoInfo` <-
function(file=logRoot())paste(system(paste("svn info --xml",file),intern=TRUE),collapse="")

