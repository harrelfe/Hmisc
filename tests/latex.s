require(Hmisc)
x <- cbind(x1=1:5, x2=2:6)
file <- '/tmp/z.tex'
# Note: adding here package caused LaTeX problems
cat('\\documentclass{article}\n\\usepackage{hyperref,lscape,ctable,booktabs,longtable}\n\\begin{document}\n', file=file)

test <- function(caption=NULL, center=NULL, table.env=TRUE, size=NULL,
                 booktabs=FALSE, landscape=FALSE, ctable=FALSE, longtable=FALSE,
                 hyperref=NULL, insert=TRUE) {
  i <<- i + 1
  cat('\\clearpage\ni=', i, '\n\\hrule\n', sep='', file=file, append=TRUE)
  ib <- it <- NULL
  g <- function(x) {
    if(! length(x)) return(NULL)
    if(is.character(x)) paste(substitute(x), '=', x, ', ', sep='')
    else if(x) paste(substitute(x), '=T, ', sep='')
    else NULL
  }
  if(insert) {
    z <- paste(g(caption), g(center), g(table.env), g(size), g(booktabs),
               g(landscape), g(ctable), g(longtable), g(hyperref), sep='')
    it <- paste('Top: i=', i, ':', z, sep='')
    ib <- 'Text for bottom'
  }
  w <- latex(x, file=file, append=TRUE,
             caption=caption, center=center, table.env=table.env,
             size=size, booktabs=booktabs, landscape=landscape,
             ctable=ctable, longtable=longtable, hyperref=hyperref,
             insert.top=it, insert.bottom=ib)
  invisible()
}

i <- 0
test()
test(hyperref='rrrrr')
test(caption='This caption')
for(cen in c('center', 'centering', 'centerline')) test(center=cen)
test(table.env=FALSE)
test(size='scriptsize')
test(table.env=FALSE)
test(booktabs=TRUE, landscape=TRUE)
test(ctable=TRUE, landscape=TRUE)
test(longtable=TRUE)

cat('\\end{document}\n', file=file, append=TRUE)
# Run pdflatex /tmp/z
