## From Bayazid Sarkar <sarkarbayazid@gmail.com>
require(Hmisc)
set.seed(1)
x <- exp(rnorm(100))
w <- sample(1:5, 100, TRUE)
g <- sample(c('a','b','c'), 100, TRUE)

Ecdf(log(x), weights=w, lty=1:3, col=1:3, group=g, label.curves=list(keys=1:3),
     subtitles=FALSE)

Ecdf(x, weights=w, lty=1:3, col=1:3, group=g, label.curves=list(keys=1:3),
     subtitles=FALSE, log='x')
