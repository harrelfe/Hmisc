# Author: Frank Harrell 24 Jun 91
xy.group <- function(x,y,m=150,g,fun=mean,result="list")	{

k <- !is.na(x+y)
if(sum(k)<2)stop("fewer than 2 non-missing x and y")
x <- x[k]
y <- y[k]
if(missing(m)) q <- cut2(x,g=g,levels.mean=TRUE,digits=7) else
	q <- cut2(x,m=m,levels.mean=TRUE,digits=7)
n <- table(q)
x.mean <- as.single(levels(q))
y.fun <- as.vector(tapply(y, q, fun))
if(result=="matrix")	{
	z <- cbind(table(q),x.mean,y.fun)
	dimnames(z) <- list(levels(q), c("n","x","y"))
			}	else
	z <- list(x=x.mean,y=y.fun)
z								}
