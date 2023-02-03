`logAppendix` <- 
function (
	object = logSummary()[, c('file', 'revf', 'reviewer', 'time')],
        dir = getwd(), 
	file = file.path(dir, 'review.tex'),
        label = 'review', 
	rowname = NULL,
        caption = 'Summary of file review: file, last version reviewed (revf), system identifier of reviewer, and time of acceptance.',
        caption.lot = 'Summary of file review', 
	where = 'H',
        show = TRUE,
        ...
){
        library(Hmisc)
        object[] <- lapply(object, latexTranslate)
        if (nrow(logPending()))warning('see logPending()')
        out <- latex(
            object = object, 
	    file = file, 
	    label = label, 
	    rowname = rowname,
            caption = caption, 
	    caption.lot = caption.lot, 
	    where = 'H',
	    ...
	)
        if(show) print(out)
        invisible(out)
}
