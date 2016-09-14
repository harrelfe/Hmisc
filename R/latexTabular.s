latexTabular <- function(x, headings=colnames(x),
                         align =paste(rep('c',ncol(x)),collapse=''),
                         halign=paste(rep('c',ncol(x)),collapse=''),
                         helvetica=TRUE, translate=TRUE, hline=0, center=FALSE,
                         ...)
{
  if(! (is.matrix(x) || is.data.frame(x))) x <- as.matrix(x)
  nc <- ncol(x)
  if(translate)
    for(i in 1 : nc)
      if(is.factor(x[, i]) || is.character(x[, i]))
        x[, i] <- latexTranslate(x[, i])

  if(length(list(...))) x <- format.df(x, ...)

  xhalign <- substring(halign, 1:nchar(halign), 1:nchar(halign))
  w <- paste0(if(center) '\\begin{center}',
              '\\begin{tabular}{', align, '}')
  if(hline == 2) w <- paste0(w, '\\hline')
  if(helvetica)  w <- paste('{\\fontfamily{phv}\\selectfont', w)
  if(length(headings)) {
    h <- strsplit(headings, split='\n')
    ## strsplit returns character(0) for ""
    for(i in 1 : length(h)) if(! length(h[[i]])) h[[i]] <- ''

    maxl <- max(sapply(h, length))
    H <- character(maxl)
    for(i in 1 : maxl) {
      lab <- sapply(h, function(x) if(length(x) < i) '' else x[i])
      if(translate) lab <- latexTranslate(lab)
      H[i] <- if(halign != align)
                paste0(paste(paste0('\\multicolumn{1}{', xhalign, '}{', 
                                  lab, '}'),
                            collapse='&'), '\\\\')
              else paste0(paste(lab, collapse='&'), '\\\\')
    }
    H <- paste(H, collapse='\n')
    if(hline > 0) H <- paste0(H, '\\hline')
  }
  v <- paste0(apply(x, 1, paste, collapse='&'), '\\\\')
  if(hline == 2) v <- c(v, '\\hline')
  v <- paste(v, collapse='\n')
#  v <- paste(paste0(v, '\\\\'), if(hline == 2) '\\hline'), collapse='\n')
  if(length(headings)) v <- paste(H, v, sep='\n')
  paste0(w, '\n', v, '\n\\end{tabular}',
         if(center) '\n\\end{center}',
         if(helvetica) '}')
}
