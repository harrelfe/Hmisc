latexTabular <- function(x, headings=colnames(x),
                         align =paste(rep('c',ncol(x)),collapse=''),
                         halign=paste(rep('c',ncol(x)),collapse=''),
                         helvetica=TRUE, translate=TRUE, hline=0, ...)
{
  if(! (is.matrix(x) || is.data.frame(x))) x <- as.matrix(x)
  nc <- ncol(x)
  if(translate)
    for(i in 1 : nc)
      if(is.factor(x[, i]) || is.character(x[, i]))
        x[, i] <- latexTranslate(x[, i])

  if(length(list(...))) x <- format.df(x, ...)

  xhalign <- substring(halign, 1:nchar(halign), 1:nchar(halign))
  w <- paste('\\begin{tabular}{', align, '}', sep='')
  if(hline == 2) w <- paste(w, '\\hline', sep='')
  if(helvetica) w <- paste('{\\fontfamily{phv}\\selectfont', w, sep='')
  if(length(headings)) {
    if(translate) headings <- latexTranslate(headings)
    h <- if(halign != align)
      paste(paste(paste('\\multicolumn{1}{', xhalign, '}{', 
                                       headings, '}',sep=''),
                                 collapse='&'), '\\\\', sep='')
    else paste(paste(headings, collapse='&'), '\\\\', sep='')
  }
  if(hline == 1) h <- paste(h, '\\hline', sep='')
  if(hline == 2) h <- paste(h, '\\hline\\hline', sep='')
  v <- apply(x, 1, paste, collapse='&')
  v <- paste(paste(v, '\\\\', if(hline == 2) '\\hline'), collapse='\n')
  if(length(headings)) v <- paste(h, v, sep='\n')
  paste(w, v, '\\end{tabular}', if(helvetica)'}', sep='\n')
}
