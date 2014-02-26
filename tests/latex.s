require(Hmisc)
x <- cbind(x1=1:5, x2=2:6)
f <- '/tmp/z.tex'
# Note: adding here package caused LaTeX problems
cat('\\documentclass{article}\n\\usepackage{hyperref,lscape,ctable,booktabs,longtable}\n\\begin{document}\n', file=f)
w <- latex(x, file=f, append=TRUE)
w <- latex(x, hyperref='rrrrr', file=f, append=TRUE)
w <- latex(x, caption='This caption', file=f, append=TRUE)
for(cen in c('center', 'centering', 'centerline'))
  w <- latex(x, center=cen, file=f, append=TRUE)
w <- latex(x, table.env=FALSE, append=TRUE, file=f)
w <- latex(x, size='scriptsize', file=f, append=TRUE)
w <- latex(x, table.env=FALSE, insert.bottom='Hello there', file=f, append=TRUE)
w <- latex(x, booktabs=TRUE, landscape=TRUE, file=f, append=TRUE)
w <- latex(x, ctable=TRUE, landscape=TRUE, file=f, append=TRUE)
w <- latex(x, longtable=TRUE, file=f, append=TRUE)
cat('\\end{document}\n', file=f, append=TRUE)
# Run pdflatex /tmp/z
