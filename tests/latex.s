require(Hmisc)
x <- cbind(x1=1:5, x2=2:6)
f <- '/tmp/z.tex'
cat('\\documentclass{article}\n\\usepackage{lscape,ctable,booktabs}\n\\begin{document}\n', file=f)
w <- latex(x, booktabs=TRUE, landscape=TRUE, file=f, append=TRUE)
w <- latex(x, ctable=TRUE, landscape=TRUE, file=f, append=TRUE)
cat('\\end{document}\n', file=f, append=TRUE)
# Run pdflatex /tmp/z
