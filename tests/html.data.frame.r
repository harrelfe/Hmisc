require(Hmisc)
getHdata(pbc)
f <- '/tmp/z.html'
cat('', file=f)
source('~/R/Hmisc/R/html.s');source('~/R/Hmisc/R/describe.s')
h <- html(contents(pbc), levelType='table', file=f, append=TRUE)
d <- describe(pbc)
cat(html(d), sep='\n', file=f, append=TRUE)
tab <- '<table><tr><th>X</th><th>Y</th></tr><tr><td>1</td><td>b</td></tr></table>'
d <- data.frame(x=runif(4), y=letters[1:4])
h <- htmlTable::htmlTable(d)
cat(h)
z <- c(html(d, border=2, file=FALSE), '<hr>',
       tab,
       html(d, border=0, file=FALSE), '<hr>',
       html(d, border=1, file=FALSE) )

cat(z, sep='\n', file=f, append=TRUE)

       
